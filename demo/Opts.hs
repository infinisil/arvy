module Opts where

import           Arvy.Algorithm
import           Arvy.Algorithm.Collection
import           Arvy.Log
import           Data.List
import           GHC.Word
import           Options.Applicative
import qualified Parameters.Tree           as Tree
import           Polysemy
import           Polysemy.RandomFu

data Options = Options
  { optNodeCount  :: NodeCount
  , optAlg        :: String
  , optTree       :: String
  , optReqsPerSec :: Float
  , optDemo       :: Bool
  , optSeed       :: Word32
  , optTitle      :: String
  }

getAlg :: Options -> GeneralArvy '[Log]
getAlg Options { optAlg = name } = case lookup name mapping of
  Nothing -> error $ "No such algorithm, choices are " ++ intercalate ", " (map fst mapping)
  Just alg -> alg
  where mapping =
          [ ("arrow", arrow)
          , ("ivy", ivy)
          , ("edgemin", minWeight)
          , ("pairmin", localMinPairs)
          , ("dynstar", dynamicStar)
          , ("ratio", inbetween (1 % 2))
          ]

getTree :: Options -> Tree.TreeParam '[Log, RandomFu, Lift IO]
getTree Options { optTree = name } = case lookup name mapping of
  Nothing -> error $ "No such tree, choices are " ++ intercalate ", " (map fst mapping)
  Just alg -> alg
  where mapping =
          [ ("random", Tree.random)
          , ("mst", Tree.mst)
          , ("minpair", Tree.shortPairs)
          , ("approxminpair", Tree.shortestPairs)
          , ("star", Tree.bestStar)
          , ("ring", Tree.ring)
          ]

parser :: Parser Options
parser = Options
  <$> option auto ( short 'n'
                  <> long "node-count"
                  <> value 70
                  <> help "The number of nodes to use"
                  )
  <*> option str ( short 'a'
                  <> long "algorithm"
                  <> help "Which algorithm to use"
                  )
  <*> option str ( short 't'
                  <> long "tree"
                  <> value "random"
                  <> help "Which initial tree to use"
                  )
  <*> option auto ( long "reqsPerSec"
                  <> help "For autorandom requests, how many should be issued per simulation second"
                  <> value 2
                  )
  <*> flag False True ( short 'd'
                      <> long "demo"
                      <> help "Enable demo mode with a very simple graph"
                      )
  <*> option auto ( short 's'
                  <> long "seed"
                  <> value 14
                  <> help "Random seed"
                  )
  <*> option str ( long "title"
                  <> help "Title to use"
                  <> value ""
                  )


options :: ParserInfo Options
options = info (parser <**> helper)
   ( fullDesc
  <> progDesc "Arvy demo"
   )

getOptions :: IO Options
getOptions = customExecParser p options where
  p = prefs showHelpOnEmpty
