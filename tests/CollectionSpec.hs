module CollectionSpec where


import           Arvy.Algorithm.Collection
import           Arvy.Local
import           Arvy.Pure
import qualified Parameters.Weights        as Weights
import           Polysemy
import           Test.Hspec
import Polysemy.RandomFu
import qualified Data.Vector as V
import Data.Word
import Control.Monad.ST
import Utils
import System.Random.MWC
import Data.Random.Distribution.Uniform
import Control.Monad
import Data.Ratio
import qualified Parameters.Tree as Tree
import Data.Array.IArray
import Data.List

ringn :: NodeCount -> GraphWeights
ringn n = run $ Weights.weightsGet Weights.ring n

randomRequests :: NodeCount -> Int -> [Node]
randomRequests n count = runST $ runM $ runRandomPure 0 $ do
  replicateM count (sampleRVar (integralUniform 0 (n - 1)))

collectionSpec :: Spec
collectionSpec = do
  describe "Arvy.Collection.arrow" $
    it "should not modify the tree" $ do
      let tree = [2, 1, 1, 4, 1]
          requests = randomRequests (length tree) 10
      snd (runArvyLocalPureStateless (ringn 5) tree arrow requests) `shouldBe` tree

  describe "Arvy.Collection.ivy" $ do
    it "should connect everything to the new root" $ do
      let tree = [1, 2, 3, 4, 4]
      snd (runArvyLocalPureStateless (ringn 5) tree ivy [0]) `shouldBe` [0, 0, 0, 0, 0]

  describe "Arvy.Collection.half" $ do
    it "should connect to the halfway node" $ do
      let tree = [1, 2, 3, 4, 5, 6, 7, 8, 9, 9]
      snd (runArvyLocalPureStateless (ringn 10) tree half [0]) `shouldBe` [0, 0, 0, 1, 1, 2, 2, 3, 3, 4]

  describe "Arvy.Collection.inbetween" $ do
    it "should connect to the node two fifths of the way" $ do
      let tree = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 11]
      snd (runArvyLocalPureStateless (ringn 12) tree (inbetween (2 % 5)) [0]) `shouldBe` [0, 0, 0, 1, 1, 1, 2, 2, 3, 3, 3, 4]
    it "is the same as half for 1 % 2" $ do
      let tree = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 11]
      snd (runArvyLocalPureStateless (ringn 12) tree (inbetween (1 % 2)) [0]) `shouldBe`
        snd (runArvyLocalPureStateless (ringn 12) tree half [0])
    it "is the same as arrow for 1 % 1" $ do
      let tree = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 11]
      snd (runArvyLocalPureStateless (ringn 12) tree (inbetween (1 % 1)) [0]) `shouldBe`
        snd (runArvyLocalPureStateless (ringn 12) tree arrow [0])
    it "is the same as ivy for 0 % 1" $ do
      let tree = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 11]
      snd (runArvyLocalPureStateless (ringn 12) tree (inbetween (0 % 1)) [0]) `shouldBe`
        snd (runArvyLocalPureStateless (ringn 12) tree ivy [0])

  describe "Arvy.Collection.genArrow" $ do
    it "doesn't change an MST" $ do
      let n = 300
          weights = runST $ runM $ runRandomPure 0 (Weights.weightsGet (Weights.unitEuclidian 3) n)
          tree = elems $ fst $ run $ Tree.initialTreeGet Tree.mst n weights
      treeEdges (snd (runArvyLocalPureStateless weights tree genArrow (randomRequests n 1000))) `shouldBe` treeEdges tree

treeEdges :: [Node] -> [Edge]
treeEdges tree = sort $ concat $ zipWith (\a b -> [ (min a b, max a b) | a /= b ]) [0..] tree

runRandomPure :: Member (Lift (ST s)) r => Word32 -> Sem (RandomFu ': r) a -> Sem r a
runRandomPure seed sem = do
  gen <- sendM $ initialize (V.singleton seed)
  runRandomSource' gen sem
