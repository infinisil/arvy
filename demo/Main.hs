{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
module Main where

{-
TODO:
- Make arrows point to node circle edge instead of center
- Implement auto random requests (by pressing r)
-}

import           Arvy.Algorithm
import           Arvy.Algorithm.Collection
import           Control.Applicative
import           Control.Lens
import           Data.Array.Unboxed
import           Data.Bifunctor
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.Ord                             (comparing)
import           Evaluation.Types
import           GHC.Float
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Graphics.Gloss.Geometry.Angle
import           Graphics.Gloss.Interface.IO.Interact
import qualified Parameters.Tree                      as Tree
import qualified Parameters.Weights                   as Weights
import           Polysemy
import           Polysemy.State
import           Utils

data Message p = Message
  { _sender   :: Node
  , _receiver :: Node
  , _progress :: Float
  , _payload  :: p
  }

data RequestPayload msg = RequestPayload
  { _requester       :: Node
  , _messageContents :: msg Node
  }

makeLenses ''RequestPayload

type RequestMessage msg = Message (RequestPayload msg)
type TokenMessage = Message ()

makeLenses ''Message

data TokenState
  = HasToken
  | WantsToken
  | ManyWantToken
    { parent :: Node
    , next   :: Node
    }
  | Idle
    { parent :: Node
    }


data NodeState s = NodeState
  { _tokenState :: TokenState
  , _algState   :: s
  }

makeLenses ''NodeState

data PendingMessages msg = PendingMessages
  { _requestMessages :: [RequestMessage msg]
  , _tokenMessage    :: Maybe TokenMessage
  }

makeLenses ''PendingMessages

data DrawState msg s = DrawState
  { _nodeStates      :: Array Node (NodeState s)
  , _pendingMessages :: PendingMessages msg
  , _viewSize        :: (Int, Int)
  }

makeLenses ''DrawState

squareSize :: DrawState msg s -> Float
squareSize DrawState { _viewSize } = fromIntegral $ uncurry min _viewSize

type Points = Array Int Point

draw :: Points -> DrawState msg s -> Picture
draw points state@DrawState { .. } = convert $ nodes <> messages where
  square = squareSize state

  convert :: Picture -> Picture
  convert = scale square square . translate (-0.5) (-0.5)

  nodes :: Picture
  nodes = mconcat $ map (uncurry drawNode) (assocs _nodeStates)

  moveToPoint :: Point -> Picture -> Picture
  moveToPoint = uncurry translate

  tip = scale 0.004 0.004 $ polygon [(0,0), (2,-1), (2,1)]

  drawArrow :: Node -> Node -> Picture
  drawArrow fromNode toNode = line [x, y] <> arrowTip where
    x@(x1, y1) = points ! fromNode
    y@(x2, y2) = points ! toNode

    angle = radToDeg (normalizeAngle (atan2 (x1 - x2) (y1 - y2))) - 90
    arrowTip = moveToPoint y $ rotate angle tip

  drawNode :: Node -> NodeState s -> Picture
  drawNode node NodeState { _tokenState = token } = go token where
    dot = moveToPoint (points ! node) $ circleSolid 0.004
    go :: TokenState -> Picture
    go HasToken                 = color green dot
    go WantsToken               = color red dot
    go (ManyWantToken parent _) = drawArrow node parent <> color red dot
    go (Idle parent)            = drawArrow node parent <> dot

  messages :: Picture
  messages = color blue (mconcat (map drawMessage (state ^. pendingMessages . requestMessages)))
    <> maybe mempty (color green . drawMessage) (state ^. pendingMessages . tokenMessage)

  drawMessage :: Message p -> Picture
  drawMessage Message { .. } = translate x y $ circleSolid 0.004 where
    (x1, y1) = points ! _sender
    (x2, y2) = points ! _receiver

    x = x1 + _progress * (x2 - x1)
    y = y1 + _progress * (y2 - y1)


data NodeEvent msg = MakeRequest
                   | RequestReceive (RequestPayload msg)
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
          , _progress = 0.0
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
          , _progress = 0.0
          , _payload = ()
          }
    return (Idle parent, Nothing, Just tokenMsg)
  go (ManyWantToken parent next) (RequestReceive RequestPayload { _messageContents, _requester }) = do
    (newParent, newMsg) <- runner $ arvyForwardRequest _messageContents node parent
    let requestMsg = Message
          { _sender = node
          , _receiver = parent
          , _progress = 0.0
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
          , _progress = 0.0
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
          , _progress = 0.0
          , _payload = RequestPayload
            { _requester = node
            , _messageContents = msg
            }
          }

    return (WantsToken, Just requestMsg, Nothing)

messageDistance :: GraphWeights -> Message p -> Float
messageDistance weights Message { _sender, _receiver } = double2Float $ weights ! (_sender, _receiver)

passTime :: GraphWeights -> PendingMessages msg -> Float -> (PendingMessages msg, [(Node, NodeEvent msg)])
passTime weights PendingMessages { .. } dt = (PendingMessages stillPendingRequests newTokenMessage, events) where
  (receivedMessages, stillPendingRequests) = partitionEithers $ map advanceMessage _requestMessages
  tokenEvent :: Either (Node, NodeEvent msg) (Maybe TokenMessage)
  tokenEvent = first (second (const TokenReceive)) $ traverse advanceMessage _tokenMessage

  newTokenMessage = fromRight Nothing tokenEvent
  --token = maybeToList $ fmap (undefined . advanceMessage) _tokenMessage
  events = either (:[]) (const []) tokenEvent ++ map (second RequestReceive) receivedMessages

  advanceMessage :: Message p -> Either (Node, p) (Message p)
  advanceMessage msg@Message { _receiver, _progress, _payload }
    | newProgress >= 1.0 = Left (_receiver, _payload)
    | otherwise = Right msg { _progress = newProgress }
    where newProgress = _progress + dt / messageDistance weights msg

main :: IO ()
main = runAlg minWeight

runAlg :: GeneralArvy '[] -> IO ()
runAlg (GeneralArvy spec) = runSpec spec where
  runSpec :: ArvySpec () Node '[] -> IO ()
  runSpec ArvySpec { .. } = runBehavior arvyBehavior arvyInitState arvyRunner


nearest :: DrawState msg s -> Points -> (Float, Float) -> Int
nearest (squareSize -> square) points (clickx, clicky) = near where
  x = clickx / square + 0.5
  y = clicky / square + 0.5
  near = fst $ minimumBy (comparing (dist . snd)) (assocs points)
  dist :: Point -> Float
  dist (x1, y1) = (x1 - x) ** 2 + (y1 - y) ** 2


runBehavior
  :: forall msg s r'
   . ArvyBehavior Node msg r'
  -> (NodeCount -> ArvyNodeData () -> Sem '[] s)
  -> (forall x . UArray Node Weight -> Sem r' x -> Sem '[State s] x)
  -> IO ()
runBehavior behavior initState runner = do

  let n = 1000
      seed = 0

  pointsRaw <- runM $ runRandomSeed seed 0 $ Weights.randomPoints n 2

  let weights = Weights.pointWeights n 2 pointsRaw
      points = amap (\arr -> (double2Float $ arr ! 0, double2Float $ arr ! 1)) pointsRaw

  initialTree <- runM $ runRandomSeed seed 1 $ Tree.treeGen Tree.ring n weights


  let initialDrawState = DrawState
        { _nodeStates = listArray (0, n - 1)
          [ NodeState
            { _tokenState = if parent == node
              then HasToken
              else Idle parent
            , _algState = run $ initState n (ArvyNodeData parent (\u -> weights ! (node, u)) ())
            }
          | node <- [0..n-1]
          , let parent = initialTree ! node
          ]
        , _pendingMessages = PendingMessages
          { _requestMessages = []
          , _tokenMessage = Nothing
          }
        , _viewSize = (1, 1)
        }

  let

      runit :: forall x . Node -> Sem r' x -> Sem '[State s] x
      runit node = runner $ listArray (0, n-1) [ weights ! (node, end) | end <- [0..n-1]]

      handle :: Event -> DrawState msg s -> DrawState msg s
      handle (EventKey (MouseButton LeftButton) Down (Modifiers Up Up Up) (x, y)) state =
        processEvent node MakeRequest state
        where node = nearest state points (x, y)
      handle (EventResize newSize) state = state & viewSize .~ newSize
      handle _ state = state

      processEvent ::  Node -> NodeEvent msg -> DrawState msg s -> DrawState msg s
      processEvent node event state = state
        & nodeStates . ix node .~ NodeState newTokenState newAlgState
        & pendingMessages . requestMessages %~ (++ maybeToList mReqMsg)
        & pendingMessages . tokenMessage %~ (<|> mTokMsg)
        where
          NodeState { .. }  = (state ^. nodeStates) ! node
          (newAlgState, (newTokenState, mReqMsg, mTokMsg)) = run $ runState _algState $ nodeStateTransition behavior (runit node) node _tokenState event

      speed :: Float
      speed = 10

      step :: Float -> DrawState msg s -> DrawState msg s
      step dt state = state
        & pendingMessages .~ stillPending
        & \initialState -> foldl (\oldState (node, event) -> processEvent node event oldState) initialState events
        where
        (stillPending, events) = passTime weights (state ^. pendingMessages) (dt * speed)

  play FullScreen white 100 initialDrawState (draw points) handle step
