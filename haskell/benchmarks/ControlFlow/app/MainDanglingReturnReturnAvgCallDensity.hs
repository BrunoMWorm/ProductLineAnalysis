{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

#define RETURN
-- #define RETURN_AVG
-- #define DANGLING_SWITCH
-- #define CALL_DENSITY

module MainDanglingReturnReturnAvgCallDensity where

import CFG (CFG (CFG), CFGNode (CFGNode))
import CFGParser (readCFG)
import qualified Control.Arrow as Data.Bifunctor
import Control.DeepSeq (NFData)
import Criterion (bench, bgroup, env, nf)
import Criterion.Main
import Criterion.Main.Options
import Data.List (sortOn)
import GHC.Generics (Generic)
import GHC.IO.Handle (hClose)
import GHC.IO.IOMode (IOMode (ReadMode))
import Language.C.System.Preprocess (CppArgs (inputFile))
import Memoization.Core.Memory
import Memoization.Core.State
import Options.Applicative
import PresenceCondition (getAllConfigs)
import SPL (Var (Var), compact, getFeatures, groupVals, liftV, ttPC, (^|))
import Serialization.StoreBDD (loadMemory, storeMemory)
import System.IO (hGetContents, openFile)
import System.Timeout (timeout)
import System.Directory (doesFileExist)
import qualified VCFG as V

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
analysis = "ReturnAverage"
#endif

#ifdef CALL_DENSITY
import qualified CallDensityDeep as Deep
import qualified CallDensityDeepMemo as DeepMemo
analysis = "CallDensity"
#endif

changedFunctionNamesDir :: String
changedFunctionNamesDir = "inputs/results"

readfileContents :: FilePath -> IO [String]
readfileContents filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
    then do
      handle <- openFile filePath ReadMode
      contents <- hGetContents handle
      return (lines contents)
    else return []

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
    fileversion :: String,
    previousversion :: String,
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
      <*> strOption
        ( long "fileversion"
            <> value "version of the input file"
            <> metavar "STR"
            <> help "Version of the file to process."
        )
      <*> strOption
        ( long "previousversion"
            <> value "previous version of the input file for reusing the values"
            <> metavar "STR"
            <> help "Previous version of the file analysed."
        )
      <*> parseWith defaultConfig

-- Auxiliary function for removing the 'quotes' and 'slash' characters from the string stored in memory
-- representing the function name
extractFnName :: [Char] -> [Char]
extractFnName = filter (not . (`elem` "\\\'\""))

compactOnEq :: Eq t => Var t -> Var t
compactOnEq vri = let (Var vc) = compact vri in compact (Var (groupVals vc (==)))

main :: IO ()
main = do
  cliParams <- execParser $ describeWith customParser
  let fileName = filename cliParams
  let fileVersion = fileversion cliParams
  let previousVersion = previousversion cliParams

  let cfgInputFile = "artifacts/" <> fileVersion <> "/cfgs/original/" <> fileName
  let memoizationDir = "artifacts/" <> fileVersion <> "/memoization/" <> fileName <> "/" <> analysis <> "/"
  let memoizationValuesFileName = fileName <> ".memo"
  let previousMemoizationDir = "artifacts/" <> previousVersion <> "/memoization/" <> fileName <> "/" <> analysis <> "/"

  correctnessRunEnv <- setupEnv cfgInputFile

  let reconstructedMemoryIO :: IO DeepMemo.MemoryConc
      reconstructedMemoryIO = loadMemory previousMemoizationDir memoizationValuesFileName
  reconstructedMemory <- reconstructedMemoryIO
  print "Reconstructed Memory:"
  print reconstructedMemory

  let changedFunctionNamesDir = "artifacts/" <> fileVersion <> "/changed_functions/"
  modifiedFunctions <- readfileContents (changedFunctionNamesDir <> fileName)
  let validMemory = filter (\(k, v) -> extractFnName (fst k) `notElem` modifiedFunctions) reconstructedMemory
  print "Valid Memory:"
  print validMemory

  let origRes = deep (deepCFG correctnessRunEnv)
  print "Original Analysis:"
  print origRes

  let (memoRes, newMemory) = (memoResRaw, map (Data.Bifunctor.second compactOnEq) memoryRaw)
        where
          (memoResRaw, memoryRaw) = deepMemoSt validMemory (deepCFG correctnessRunEnv)
  print "Deep Analysis:"
  print memoRes
  storeMemory memoizationDir memoizationValuesFileName newMemory

  print "Are equal?"
  print (show origRes == show memoRes)

  runMode
    (others cliParams)
    [ env (setupEnv cfgInputFile) $ \env ->
        let loadMemoryIO :: String -> IO DeepMemo.MemoryConc
            loadMemoryIO = loadMemory previousMemoizationDir
            storeMemoryIO :: DeepMemo.MemoryConc -> IO ()
            storeMemoryIO = storeMemory memoizationDir memoizationValuesFileName
         in bgroup
              (fileVersion <> "-" <> fileName)
              [ 
                bench "deep" $ nf deep (deepCFG env),
                bench "deepMemo" $ nf (deepMemo validMemory) (deepCFG env),
                bench "loadMemory" $ nfIO (loadMemoryIO memoizationValuesFileName),
                bench "storeMemory" $ nfIO (storeMemoryIO newMemory)
              ]
    ]