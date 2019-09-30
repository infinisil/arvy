{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{- |
Description : Arvy algorithm abstraction
Copyright   : (c) Silvan Mosberger, 2019
License     : GPL-3
Maintainer  : contact@infinisil.com
Stability   : experimental

This module contains an abstract definition of an Arvy algorithm (Pankaj Khanchandani and Roger Wattenhofer \"/The Arvy Distributed Directory Protocol/\", The 31st ACM Symposium on Parallelism in Algorithms and Architectures, Pages 225-235, <https://dl.acm.org/citation.cfm?id=3323181>).
-}

module Arvy.Algorithm
  ( StaticArvyBehavior(..)
  , StaticArvySpec(..)
  , ArvyBehavior(..)
  , ArvySpec(..)
  , Forwardable(..)
  , NodeIndex(..)
  , ArvyData(..)
  , ArvyNodeData(..)
  , ArvyAlgorithm(..)
  , Node
  , NodeCount
  ) where

import Polysemy
import Polysemy.State

{- |
An Arvy behavior for a static Arvy algorithm. This is not much of a heuristic because there's only one possible static Arvy algorithm. However, this allows additional code to be ran on events and custom messages to be passed.

- @i@ is the node index type, this should stay polymorphic.
- @msg :: * -> *@ is the type of request messages passed between nodes, parametrized by the node index type.
- @r@ is the effects the algorithm runs in, which can include effects parametrized by @i@.
-}
data StaticArvyBehavior i (msg :: * -> *) r = StaticArvyBehavior
  { staticArvyMakeRequest :: i -> i -> Sem r (msg i)
  -- ^ @'staticArvyMakeRequest' cur succ@ determines what message should be sent to the successor node @succ@ when some node @cur@ makes a request for the token.
  , staticArvyForwardRequest :: msg i -> i -> i -> Sem r (msg i)
  -- ^ @'staticArvyForwardRequest' msg cur succ@ determines what message should be forwarded to the successor node @succ@ when some node @cur@ received a token request message @msg@.
  , staticArvyReceiveRequest :: msg i -> i -> Sem r ()
  -- ^ @'staticArvyReceiveRequest' msg cur@ determines what the receiving node should do with the received message, if anything.
  }

-- | A specification for how to execute a static arvy algorithm. @a@ is the type of data a node needs in order to run.
data StaticArvySpec a r = forall msg r' . StaticArvySpec
  { staticArvyBehavior :: forall i . NodeIndex i => StaticArvyBehavior i msg r'
  -- ^ How the algorithm should behave for certain events occuring.
  , staticArvyRunner :: forall x . Node -> Sem r' x -> Sem (State a ': r) x
  -- ^ How the algorithm should reinterpret the potentially node-specific effects @r'@ into non-node-specific effects @r@. For this it receives the index of the node along with its data.
  }

{- |
An Arvy heuristic for a dynamic algorithm.

- @i@ is the node index type, this should stay polymorphic.
- @msg :: * -> *@ is the type of request messages passed between nodes, parametrized by the node index type.
- @r@ is the effects the algorithm runs in, which can include effects parametrized by @i@.
-}
data ArvyBehavior i msg r = ArvyBehavior
  { arvyMakeRequest :: i -> Succ i -> Sem r (msg i)
  -- ^ 'dynamicArvyMakeRequest cur succ' determines what message should be sent to the successor node @succ@ when some node @cur@ makes a request for the token.
  , arvyForwardRequest :: msg (Pred i) -> i -> Succ i -> Sem r (Pred i, msg i)
  -- ^ @'dynamicArvyForwardRequest' msg cur succ@ determines both what message should be forwarded to the successor node @succ@ when some node @cur@ received a token request message @msg@ and what @cur@'s new successor should be. For correctness guarantees, only previously traversed nodes can be selected. This is enforced by @i@ only allowing node indices to be forwarded one way, from @'Pred' i@ to @i@ to @'Succ' i@ (which can be done with the 'forward' function).
  , arvyReceiveRequest :: msg (Pred i) -> i -> Sem r (Pred i)
  -- ^ @'dynamicArvyReceiveRequest' msg cur@ determines what the current node @cur@'s new successor should be when a token request message @msg@ was received and @cur@ holds the token. For correctness guarantees, only previously traversed nodes can be selected. This is enforced by @i@ only allowing node indices to be forwarded one way, from @'Pred' i@ to @i@ to @'Succ' i@ (which can be done with the 'forward' function).
  }

-- | A specification for how to execute a dynamic arvy algorithm. @a@ is the type of data a node needs in order to run.
data ArvySpec a r = forall msg r' . ArvySpec
  { arvyBehavior :: forall i . NodeIndex i => ArvyBehavior i msg r'
  -- ^ How the algorithm should behave for certain events occuring.
  , arvyRunner :: forall x . Node -> Sem r' x -> Sem (State a ': r) x
  -- ^ How the algorithm should reinterpret the potentially node-specific effects @r'@ into non-node-specific effects @r@. For this it receives the index of the node along with its data.
  }


-- | A class for node indices that can be forwarded in one direction. Having this class as a constraint on types @ia@ and @ib@ is equivalent to passing a function @ia -> ib@.
class Forwardable ia ib where
  -- | Forward a node index
  forward :: ia -> ib

-- | All equivalent types can be trivially forwarded
instance Forwardable i i where
  {-# INLINE forward #-}
  forward = id

-- | A class for encoding that for a node index type @i@, there's a predecessor type @Pred i@ and a successor type @Succ i@, which can be forwarded from back to front.
class ( Forwardable (Pred i) i
      , Forwardable i (Succ i)
      , Forwardable (Pred i) (Succ i)
      ) => NodeIndex i where
  type Pred i :: *
  type Succ i :: *

-- | All types can trivially be node indices. This doesn't pose a problem since this is only used for correctness.
instance NodeIndex i where
  type Pred i = i
  type Succ i = i

type Node = Int
type NodeCount = Node

-- | The data determining the number of nodes and what data each of them should start with. @a@ is the additional algorithm-specific data each node needs. An example would be for @a@ to be @[Double]@ representing the weights to all other nodes.
data ArvyData a = ArvyData
  { arvyDataNodeCount :: NodeCount
  , arvyDataNodeData :: Node -> ArvyNodeData a
  }

-- | The data a single node should start with. @a@ is the additional algorithm-specific data each node needs. An example would be for @a@ to be @[Double]@ representing the weights to all other nodes.
data ArvyNodeData a = ArvyNodeData
  { arvyNodeDataSuccessor :: Node
  , arvyNodeDataAdditional :: a
  }

{- |
A fully specified Arvy algorithm including how to generate initalization data for all nodes and how to run the algorithm on them with the data. For @'ArvyAlgorithm' p a r@:

- @p@ stands for the parameter type needed for it to generate the initialization data for each node. For @'ArvyData' a@ this means the initialization data itself can be passed directly.
- @a@ stands for the additional algorithm-specific data each node might need.
- @r@ stands for the effects this algorithm runs in.
-}
data ArvyAlgorithm :: * -> * -> [(* -> *) -> * -> *] -> * where
  -- | An arrow algorithm, which never changes the tree.
  Arrow
    :: StaticArvySpec a r
    -> ArvyAlgorithm (ArvyData a) a r
  -- | A general dynamic Arvy algorithm which works on any graphs/trees.
  GeneralArvy
    :: ArvySpec a r
    -> ArvyAlgorithm (ArvyData a) a r
  -- | A specialized dynamic Arvy algorithm that only works on certain graphs/trees parametrized by @p@.
  SpecializedArvy
    :: (p -> Sem r (ArvyData a))
    -> ArvySpec a r
    -> ArvyAlgorithm p a r
