{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
module Main where

import           Arvy.Algorithm
import           Arvy.Log
import           Control.Applicative
import           Control.Lens
import           Data.Array.Unboxed
import           Data.Bifunctor
import qualified Data.Heap                        as H
import           Data.List
import           Data.Maybe
import           Data.Ord                         (comparing)
import           Evaluation.Types
import           GHC.Float
import           Graphics.Gloss
import           Graphics.Gloss.Geometry.Angle
import           Graphics.Gloss.Interface.IO.Game
import           Opts
import qualified Parameters.Tree                  as Tree
import qualified Parameters.Weights               as Weights
import           Polysemy
import           Polysemy.State
import           System.Exit
import           System.Random                    hiding (next)
import           Utils

{-
TODO:
- cleanup
- interactive moving of points?
-}

data Message p = Message
  { _sender   :: !Node
  , _receiver :: !Node
  , _payload  :: !p
  }

makeLenses ''Message

data RequestPayload msg = RequestPayload
  { _requester       :: !Node
  , _messageContents :: !(msg Node)
  }

makeLenses ''RequestPayload

type RequestMessage msg = Message (RequestPayload msg)
type TokenMessage = Message ()


data TokenState
  = HasToken
  | WantsToken
  | ManyWantToken
    { parent :: !Node
    , next   :: !Node
    }
  | Idle
    { parent :: !Node
    }


data NodeState s = NodeState
  { _tokenState :: !TokenState
  , _algState   :: !s
  }

makeLenses ''NodeState

data PendingMessages msg = PendingMessages
  { _requestMessages :: !(H.Heap (H.Entry Float (RequestMessage msg)))
  , _tokenMessage    :: !(Maybe (H.Entry Float TokenMessage))
  }

makeLenses ''PendingMessages

data AutoMode = NoAuto
              | AutoConcurrent !Float
              | AutoSerial

makePrisms ''AutoMode

data DrawState msg s = DrawState
  { _nodeStates      :: !(Array Node (NodeState s))
  , _pendingMessages :: !(PendingMessages msg)
  , _viewSize        :: !(Int, Int)
  , _mode            :: !AutoMode
  , _speed           :: !Float
  , _stdGen          :: !StdGen
  }

makeLenses ''DrawState

active :: DrawState msg s -> Bool
active DrawState { _pendingMessages = PendingMessages reqs Nothing }
  | H.null reqs = False
active _ = True

squareSize :: DrawState msg s -> Float
squareSize DrawState { _viewSize } = 0.8 * fromIntegral (uncurry min _viewSize)

type Points = Array Int Point

draw :: String -> Points -> GraphWeights -> DrawState msg s -> Picture
draw title points weights state@DrawState { .. } = convert $ nodes <> messages <> drawTitle where
  square = squareSize state

  drawTitle :: Picture
  drawTitle = translate (-0.1) 1.07 $ scale 0.0007 0.0007 $ text title

  size = 0.1 / sqrt (fromIntegral (rangeSize $ bounds points))

  convert :: Picture -> Picture
  convert = scale square square . translate (-0.5) (-0.55)

  nodes :: Picture
  nodes = mconcat arrowPics <> mconcat nodePics where
    (nodePics, arrowPics) = unzip $ map (uncurry drawNode) (assocs _nodeStates)

  moveToPoint :: Point -> Picture -> Picture
  moveToPoint = uncurry translate

  tip = scale size size $ polygon [(1,0), (2,-0.5), (2,0.5)]

  drawArrow :: Node -> Node -> Picture
  drawArrow fromNode toNode = line [x, y] <> arrowTip where
    x@(x1, y1) = points ! fromNode
    y@(x2, y2) = points ! toNode

    angle = radToDeg (normalizeAngle (atan2 (x1 - x2) (y1 - y2))) - 90
    arrowTip = moveToPoint y $ rotate angle tip

  redc = mixColors 7 3 red black
  bluec = mixColors 7 3 blue black
  greenc = mixColors 5 5 green black

  drawNode :: Node -> NodeState s -> (Picture, Picture)
  drawNode node NodeState { _tokenState = token } = go token where
    dot = moveToPoint (points ! node) $ circleSolid size
    go :: TokenState -> (Picture, Picture)
    go HasToken                 = (color greenc dot, mempty)
    go WantsToken               = (color redc dot, mempty)
    go (ManyWantToken parent next) = (color redc dot, drawArrow node parent <> color redc (drawArrow node next))
    go (Idle parent)            = (dot, drawArrow node parent)

  messages :: Picture
  messages = color bluec (foldMap drawMessage (state ^. pendingMessages . requestMessages))
    <> maybe mempty (color greenc . drawMessage) (state ^. pendingMessages . tokenMessage)

  drawMessage :: H.Entry Float (Message p) -> Picture
  drawMessage (H.Entry left Message { .. }) = translate x y $ scale 0.7 0.7 $ circleSolid size where
    (x1, y1) = points ! _sender
    (x2, y2) = points ! _receiver

    progress = 1 - left / double2Float (weights ! (_sender, _receiver))
    x = x1 + progress * (x2 - x1)
    y = y1 + progress * (y2 - y1)


data NodeEvent msg = MakeRequest
                   | RequestReceive !(RequestPayload msg)
                   | TokenReceive

nodeStateTransition
  :: forall msg s r' r
   . ArvyBehavior Node msg r'
  -> (forall x . Sem r' x -> Sem (State s ': r) x)
  -> Node
  -> TokenState
  -> NodeEvent msg
  -> Sem (State s ': r) (TokenState, Maybe (RequestMessage msg), Maybe TokenMessage)
nodeStateTransition ArvyBehavior { .. } runner node = go where
  go :: TokenState -> NodeEvent msg -> Sem (State s ': r) (TokenState, Maybe (RequestMessage msg), Maybe TokenMessage)
  go HasToken TokenReceive = error "Duplicate token! Received a token message but I already have a token!"
  go HasToken (RequestReceive RequestPayload { _messageContents, _requester }) = do
    newParent <- runner $ arvyReceiveRequest _messageContents node
    let tokenMsg = Message
          { _sender = node
          , _receiver = _requester
          , _payload = ()
          }
    return (Idle newParent, Nothing, Just tokenMsg)
  go HasToken MakeRequest = return (HasToken, Nothing, Nothing)

  go WantsToken TokenReceive = return (HasToken, Nothing, Nothing)
  go WantsToken (RequestReceive RequestPayload { _messageContents, _requester }) = do
    newParent <- runner $ arvyReceiveRequest _messageContents node
    return (ManyWantToken newParent _requester, Nothing, Nothing)
  go WantsToken MakeRequest = return (WantsToken, Nothing, Nothing)

  go (ManyWantToken parent next) TokenReceive = do
    let tokenMsg = Message
          { _sender = node
          , _receiver = next
          , _payload = ()
          }
    return (Idle parent, Nothing, Just tokenMsg)
  go (ManyWantToken parent next) (RequestReceive RequestPayload { _messageContents, _requester }) = do
    (newParent, newMsg) <- runner $ arvyForwardRequest _messageContents node parent
    let requestMsg = Message
          { _sender = node
          , _receiver = parent
          , _payload = RequestPayload
            { _requester = _requester
            , _messageContents = newMsg
            }
          }
    return (ManyWantToken newParent next, Just requestMsg, Nothing)
  go (ManyWantToken parent next) MakeRequest = return (ManyWantToken parent next, Nothing, Nothing)

  go (Idle _) TokenReceive = error "Received token but didn't request it!"
  go (Idle parent) (RequestReceive RequestPayload { _messageContents, _requester }) = do
    (newParent, newMsg) <- runner $ arvyForwardRequest _messageContents node parent
    let requestMsg = Message
          { _sender = node
          , _receiver = parent
          , _payload = RequestPayload
            { _requester = _requester
            , _messageContents = newMsg
            }
          }
    return (Idle newParent, Just requestMsg, Nothing)
  go (Idle parent) MakeRequest = do
    msg <- runner $ arvyMakeRequest node parent
    let requestMsg = Message
          { _sender = node
          , _receiver = parent
          , _payload = RequestPayload
            { _requester = node
            , _messageContents = msg
            }
          }

    return (WantsToken, Just requestMsg, Nothing)

messageDistance :: GraphWeights -> Message p -> Float
messageDistance weights Message { _sender, _receiver } = double2Float $ weights ! (_sender, _receiver)

reqsToRecv :: Float -> H.Heap (H.Entry Float (RequestMessage msg)) -> [(Node, NodeEvent msg)]
reqsToRecv = undefined

moveAlong :: Float -> DrawState msg s -> DrawState msg s
moveAlong dt state = state
  & pendingMessages . requestMessages %~ H.mapMonotonic (first (subtract dt))
  & pendingMessages . tokenMessage . _Just %~ first (subtract dt)
  & mode . _AutoConcurrent %~ subtract dt

passTime :: forall msg s r' . Float -> NodeCount -> GraphWeights -> ArvyBehavior Node msg r'
  -> (forall x . Node -> Sem r' x -> Sem '[State s, Log] x)
  -> Float -> DrawState msg s -> IO (DrawState msg s)
passTime reqsPerSec n weights behavior runit = advanceMsgs
  where
    advanceMsgs :: Float -> DrawState msg s -> IO (DrawState msg s)
    advanceMsgs diff state = case nextEvent state of
      Nothing          -> case state ^. mode of
        AutoSerial -> do
          let (node, stdGen') = randomNode n (state ^. stdGen)
              state' = processEvent weights behavior runit node MakeRequest $ state & stdGen .~ stdGen'
          advanceMsgs diff state'
        _ -> return state
      Just (time, simEvent)
        | time > diff -> return $ moveAlong diff state -- No messages to receive in the next diff time units, just advance them
        | otherwise -> do
            (node, event, trans) <- case simEvent of
              RandomSpawn -> do
                let (node, stdGen') = randomNode n (state ^. stdGen)
                return ( node
                       , MakeRequest
                       , (mode . _AutoConcurrent %~ (+ 1 / reqsPerSec))
                         . (stdGen .~ stdGen')
                       )
              RequestArrive (reqMsg, newMsgs) -> return
                ( reqMsg ^. receiver
                , RequestReceive (_payload reqMsg)
                , pendingMessages . requestMessages .~ newMsgs
                )
              TokenArrive tokMsg -> return
                ( tokMsg ^. receiver
                , TokenReceive
                , pendingMessages . tokenMessage .~ Nothing
                )
            advanceMsgs (diff - time)
              $ processEvent weights behavior runit node event
              $ trans
              $ moveAlong time state


type MsgHeap msg = H.Heap (H.Entry Float (RequestMessage msg))

data SimulationEvent msg = RandomSpawn
                         | RequestArrive !(RequestMessage msg, MsgHeap msg)
                         | TokenArrive !TokenMessage


nextReceive :: PendingMessages msg -> Maybe (Float, Either (RequestMessage msg, MsgHeap msg) TokenMessage)
nextReceive PendingMessages { .. } = case (H.uncons _requestMessages, _tokenMessage) of
  (Nothing, Nothing) -> Nothing
  (Nothing, Just (H.Entry tokLeft tokMsg)) -> Just (tokLeft, Right tokMsg)
  (Just (H.Entry reqLeft reqMsg, rest), Nothing) -> Just (reqLeft, Left (reqMsg, rest))
  (Just (H.Entry reqLeft reqMsg, rest), Just (H.Entry tokLeft tokMsg))
    | reqLeft < tokLeft -> Just (reqLeft, Left (reqMsg, rest))
    | otherwise -> Just (tokLeft, Right tokMsg)

nextEvent :: DrawState msg s -> Maybe (Float, SimulationEvent msg)
nextEvent state = next where
  next = listToMaybe $ sortOn fst $ catMaybes [nextRequestArrive, nextTokenArrive, nextRandomSpawn]
  nextRequestArrive = case H.uncons (state ^. pendingMessages . requestMessages) of
    Nothing                       -> Nothing
    Just (H.Entry left msg, rest) -> Just (left, RequestArrive (msg, rest))
  nextTokenArrive = case state ^. pendingMessages . tokenMessage of
    Nothing                 -> Nothing
    Just (H.Entry left msg) -> Just (left, TokenArrive msg)
  nextRandomSpawn = case state ^. mode of
    AutoConcurrent left -> Just (left, RandomSpawn)
    _                   -> Nothing


main :: IO ()
main = do
  opts <- getOptions
  runAlg opts

runAlg :: Options -> IO ()
runAlg opts@(getAlg -> GeneralArvy spec) = runSpec spec where
  runSpec :: ArvySpec () Node '[Log] -> IO ()
  runSpec ArvySpec { .. } = runBehavior opts arvyBehavior arvyInitState arvyRunner


nearest :: DrawState msg s -> Points -> (Float, Float) -> Int
nearest (squareSize -> square) points (clickx, clicky) = near where
  x = clickx / square + 0.5
  y = clicky / square + 0.55
  near = fst $ minimumBy (comparing (dist . snd)) (assocs points)
  dist :: Point -> Float
  dist (x1, y1) = (x1 - x) ** 2 + (y1 - y) ** 2

randomNode :: RandomGen g => NodeCount -> g -> (Node, g)
randomNode n g = randomR (0, n - 1) g

demoPoints :: Array Int (UArray Int Double)
demoPoints = listArray (0, 4)
  (map (\(x, y) -> listArray (0, 1) [adjustx x, adjusty y])
    [ (0, 5)
    , (7, 5)
    , (3, 4)
    , (2, 1)
    , (6, 0)
    ])
  where adjustx x = x / 7
        adjusty y = y / 5

demoTree :: RootedTree
demoTree = listArray (0, 4) [3, 2, 3, 4, 4]

runBehavior
  :: forall msg s r'
   . Options
  -> ArvyBehavior Node msg r'
  -> (NodeCount -> ArvyNodeData () -> Sem '[Log] s)
  -> (forall x . UArray Node Weight -> Sem r' x -> Sem '[State s, Log] x)
  -> IO ()
runBehavior opts@Options { optNodeCount, .. } behavior initState runner = do

  let n = if optDemo then 5 else optNodeCount

  pointsRaw <- if optDemo then return demoPoints else runM $ runRandomSeed optSeed 0 $ Weights.randomPoints n 2

  let weights = Weights.pointWeights n 2 pointsRaw
      points = amap (\arr -> (double2Float $ arr ! 0, double2Float $ arr ! 1)) pointsRaw

  initialTree <- if optDemo then return demoTree else runM $ runRandomSeed optSeed 1 $ runIgnoringLog $ Tree.treeGen (getTree opts) n weights


  let initialDrawState = DrawState
        { _nodeStates = listArray (0, n - 1)
          [ NodeState
            { _tokenState = if parent == node
              then HasToken
              else Idle parent
            , _algState = run $ runIgnoringLog $ initState n (ArvyNodeData parent (\u -> weights ! (node, u)) ())
            }
          | node <- [0..n-1]
          , let parent = initialTree ! node
          ]
        , _pendingMessages = PendingMessages
          { _requestMessages = H.empty
          , _tokenMessage = Nothing
          }
        , _viewSize = (1, 1)
        , _mode = NoAuto
        , _speed = 0.6
        , _stdGen = mkStdGen (fromIntegral optSeed)
        }

  let

      runit :: forall x . Node -> Sem r' x -> Sem '[State s, Log] x
      runit node = runner $ listArray (0, n-1) [ weights ! (node, end) | end <- [0..n-1]]

      handle :: Event -> DrawState msg s -> IO (DrawState msg s)
      handle (EventKey (MouseButton LeftButton) Down _ click) state =
        return $ processEvent weights behavior runit node MakeRequest state
        where node = nearest state points click
      handle (EventKey (MouseButton WheelUp) _ _ _) state = return $ state & speed *~ 1.1
      handle (EventKey (MouseButton WheelDown) _ _ _) state = return $ state & speed //~ 1.1
      handle (EventKey (Char 'r') Down _ _) state = return $ initialDrawState
        & viewSize .~ state ^. viewSize
        & mode .~ state ^. mode
        & speed .~ state ^. speed
      handle (EventKey (Char 'a') Down _ _) state = return $ state & mode .~ case state ^. mode of
        NoAuto -> AutoSerial
        _      -> NoAuto
      handle (EventKey (Char 'c') Down _ _) state = return $ state & mode .~ case state ^. mode of
        NoAuto           -> NoAuto
        AutoSerial       -> AutoConcurrent 0.0
        AutoConcurrent _ -> AutoSerial
      handle (EventKey (Char 'q') Down _ _) _ = exitSuccess
      handle (EventResize newSize) state = return $ state & viewSize .~ newSize
      handle _ state = return state


      -- FIXME: Iteratively pass time and issue automatic requests interleaved. This then allows more sequential requests with high speeds
      step :: Float -> DrawState msg s -> IO (DrawState msg s)
      step dt' state = passTime optReqsPerSec n weights behavior runit dt state
        where dt = dt' * state ^. speed

  playIO FullScreen white 60 initialDrawState (return . draw optTitle points weights) handle step

processEvent :: GraphWeights -> ArvyBehavior Node msg r'
  -> (forall x . Node -> Sem r' x -> Sem '[State s, Log] x) -> Node -> NodeEvent msg -> DrawState msg s -> DrawState msg s
processEvent weights behavior runit node event state = state
  & nodeStates . ix node .~ NodeState newTokenState newAlgState
  & pendingMessages . requestMessages %~ maybe id (\value -> H.insert (H.Entry (messageDistance weights value) value)) mReqMsg
  & pendingMessages . tokenMessage %~ (<|> fmap (\value -> H.Entry (messageDistance weights value) value) mTokMsg)
  where
    NodeState { .. }  = (state ^. nodeStates) ! node
    (newAlgState, (newTokenState, mReqMsg, mTokMsg)) = run $ runIgnoringLog $ runState _algState $ nodeStateTransition behavior (runit node) node _tokenState event
