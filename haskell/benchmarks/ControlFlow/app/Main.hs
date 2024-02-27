{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- #define CASE_TERMINATION
#define RETURN
-- #define RETURN_AVG
-- #define GOTOS
-- #define DANGLING_SWITCH
-- #define CALL_DENSITY

module Main where

import CFG (CFG (CFG), CFGNode (CFGNode))
import CFGParser (readCFG)
import Control.DeepSeq (NFData)
import Criterion (bench, bgroup, env, nf)
import Criterion.Main
import Criterion.Main.Options
import GHC.Generics (Generic)
import GHC.IO.Handle (hClose)
import GHC.IO.IOMode (IOMode (ReadMode))
import Language.C.System.Preprocess (CppArgs (inputFile))
import Memoization.Core.Memory
import Memoization.Core.State
import Options.Applicative
import PresenceCondition (getAllConfigs)
import SPL (Var (Var), getFeatures, liftV, ttPC, (^|))
import System.IO (hGetContents, openFile)
import System.Timeout (timeout)
import qualified VCFG as V

#ifdef CASE_TERMINATION
import qualified CaseTerminationDeep as Deep
import qualified CaseTerminationDeepMemo as DeepMemo
analysis = "CaseTermination"
#endif

#ifdef DANGLING_SWITCH
import qualified DanglingSwitchDeep as Deep
import qualified DanglingSwitchDeepMemo as DeepMemo
analysis = "DanglingSwitch"
#endif

#ifdef RETURN
import qualified ReturnDeep as Deep
import qualified ReturnDeepMemo as DeepMemo
analysis = "Return"
#endif

#ifdef RETURN_AVG
import qualified ReturnAvgDeep as Deep
import qualified ReturnAvgDeepMemo as DeepMemo
analysis = "Return Average"
#endif

#ifdef GOTOS
import qualified GotosDeep as Deep
import qualified GotosDeepMemo as DeepMemo
analysis = "Goto Density"
#endif

#ifdef CALL_DENSITY
import qualified CallDensityDeep as Deep
import qualified CallDensityDeepMemo as DeepMemo
analysis = "Call Density"
#endif

originalFilesDir :: String
originalFilesDir = "inputs/busybox/cfgs-1_18_5"

evolvedFilesDir :: String
evolvedFilesDir = "inputs/busybox/cfgs-1_19_0"

filelistDir :: String
filelistDir = "inputs/busybox/filelist"

changedFunctionNamesDir :: String
changedFunctionNamesDir = "inputs/results"

readfileContents :: FilePath -> IO [String]
readfileContents filePath = do
  handle <- openFile filePath ReadMode
  contents <- hGetContents handle
  return (lines contents)

data Env = Env
  { deepCFG :: Var V.CFG,
    shallowCFG :: Var CFG,
    fileName :: String,
    features :: [String],
    configs :: Int,
    nodeCount :: Int,
    hdr :: String
  }
  deriving (Generic, NFData)

setupEnv :: String -> IO Env
setupEnv filename = do
  !cfg <- readCFG filename
  let !nodes = V._nodes cfg
  let !nodeCount = nodes `seq` length nodes
  !features <- cfg `seq` getFeatures
  let !deep = cfg ^| ttPC
  let !shallow@(Var sh') = V.toShallowCFG cfg
  let !featCount = deep `seq` shallow `seq` length features
  let !configCount = length (getAllConfigs features)
  let !presentConfigs = length sh'
  let !hdr =
        foldr
          (\s t -> s ++ "," ++ t)
          ""
          [filename, show nodeCount, show featCount, show configCount, show presentConfigs]
  let !env = Env deep shallow filename features configCount nodeCount hdr
  putStrLn $ "Analysis:        " ++ analysis
  putStrLn $ "File:            " ++ filename
  putStrLn $ "Node#:           " ++ show nodeCount
  putStrLn $ "Features:        " ++ show features
  putStrLn $ "Feature#:        " ++ show featCount
  putStrLn $ "Config#:         " ++ show configCount
  putStrLn $ "Present config#: " ++ show presentConfigs
  return env

deep = Deep.analyze

deepMemoSt st c = runState (DeepMemo.analyze c) st


deepMemoMem st c = execState (DeepMemo.analyze c) st

deepMemo st c = evalState (DeepMemo.analyze c) st

data CustomArgs = CustomArgs
  { filename :: String,
    others :: Mode
  }

customParser :: Parser CustomArgs
customParser =
  CustomArgs
    <$> strOption
      ( long "filename"
          <> value "input file name"
          <> metavar "STR"
          <> help "Input CFG to process."
      )
    <*> parseWith defaultConfig

main :: IO ()
main = do
  cliParams <- execParser $ describeWith customParser
  let inputFile = filename cliParams
  let originalCFG = originalFilesDir <> "/" <> inputFile
  let evolvedCFG = evolvedFilesDir <> "/" <> inputFile

  -- First, we need to obtain the initial memory. As we already computed the
  -- benchmarks on the original files, we already know the cost of computing the computation inside
  -- the ST monad with an empty initial state.
  originalEnv <- setupEnv originalCFG
  modifiedFunctions <- readfileContents (changedFunctionNamesDir <> "/" <> inputFile <> ".result")
  let initialMemory = deepMemoMem [] (deepCFG originalEnv)
      validMemory = filter (\(k, v) -> k `notElem` modifiedFunctions) initialMemory

  -- Now, before the benchmarks, we check if the results are the same
  -- Thirty minutes per analysis
  evolvedEnv <- setupEnv evolvedCFG
  print "Without memoization:"
  let withoutMemoization = deep (deepCFG evolvedEnv)
  print $ withoutMemoization
  let withMemoization = deepMemo validMemory (deepCFG evolvedEnv)
  print "With memoization:"
  print withMemoization

  let timePerFile = 1000 * 1000 * 60 * 30
  timeout timePerFile $
    runMode
      (others cliParams)
      [ env (setupEnv originalCFG) $ \env ->
          bgroup
            (hdr env)
            [ bench "originalFileDeep" $ nf deep (deepCFG env),
              bench "originalFileDeepMemo" $ nf (deepMemo []) (deepCFG env)
            ]
      ]

  timeout timePerFile $
    runMode
      (others cliParams)
      [ env (setupEnv evolvedCFG) $ \env ->
          bgroup
            (hdr env)
            [ bench "evolvedFileDeep" $ nf deep (deepCFG env),
              bench "evolvedFileDeepMemo" $ nf (deepMemo validMemory) (deepCFG env)
            ]
      ]
  print "Done."
