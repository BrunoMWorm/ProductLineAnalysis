-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE CPP #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}

-- -- #define CASE_TERMINATION
-- #define RETURN
-- -- #define RETURN_AVG
-- -- #define GOTOS
-- -- #define DANGLING_SWITCH
-- -- #define CALL_DENSITY

module Main3 where

-- import CFG (CFG (CFG), CFGNode (CFGNode))
-- import CFGParser (readCFG)
-- import Control.DeepSeq (NFData)
-- import Criterion (bench, bgroup, env, nf)
-- import Criterion.Main
-- import Criterion.Main.Options
-- import GHC.Generics (Generic)
-- import GHC.IO.Handle (hClose)
-- import GHC.IO.IOMode (IOMode (ReadMode))
-- import Language.C.System.Preprocess (CppArgs (inputFile))
-- import Memoization.Core.Memory
-- import Memoization.Core.State
-- import Options.Applicative
-- import PresenceCondition (getAllConfigs)
-- import SPL (Var (Var), getFeatures, liftV, ttPC, (^|))
-- import Serialization.StoreBDD (loadMemory, storeMemory)
-- import System.IO (hGetContents, openFile)
-- import System.Timeout (timeout)
-- import qualified VCFG as V

-- #ifdef CASE_TERMINATION
-- import qualified CaseTerminationDeep as Deep
-- import qualified CaseTerminationDeepMemo as DeepMemo
-- analysis = "CaseTermination"
-- #endif

-- #ifdef DANGLING_SWITCH
-- import qualified DanglingSwitchDeep as Deep
-- import qualified DanglingSwitchDeepMemo as DeepMemo
-- analysis = "DanglingSwitch"
-- #endif

-- #ifdef RETURN
-- import qualified ReturnDeep as Deep
-- import qualified ReturnDeepMemo as DeepMemo
-- analysis = "Return"
-- #endif

-- #ifdef RETURN_AVG
-- import qualified ReturnAvgDeep as Deep
-- import qualified ReturnAvgDeepMemo as DeepMemo
-- analysis = "Return Average"
-- #endif

-- #ifdef GOTOS
-- import qualified GotosDeep as Deep
-- import qualified GotosDeepMemo as DeepMemo
-- analysis = "Goto Density"
-- #endif

-- #ifdef CALL_DENSITY
-- import qualified CallDensityDeep as Deep
-- import qualified CallDensityDeepMemo as DeepMemo
-- analysis = "Call Density"
-- #endif

-- changedFunctionNamesDir :: String
-- changedFunctionNamesDir = "inputs/results"

-- readfileContents :: FilePath -> IO [String]
-- readfileContents filePath = do
--   handle <- openFile filePath ReadMode
--   contents <- hGetContents handle
--   return (lines contents)

-- data Env = Env
--   { deepCFG :: Var V.CFG,
--     shallowCFG :: Var CFG,
--     fileName :: String,
--     features :: [String],
--     configs :: Int,
--     nodeCount :: Int,
--     hdr :: String
--   }
--   deriving (Generic, NFData)

-- setupEnv :: String -> IO Env
-- setupEnv filename = do
--   !cfg <- readCFG filename
--   let !nodes = V._nodes cfg
--   let !nodeCount = nodes `seq` length nodes
--   !features <- cfg `seq` getFeatures
--   let !deep = cfg ^| ttPC
--   let !shallow@(Var sh') = V.toShallowCFG cfg
--   let !featCount = deep `seq` shallow `seq` length features
--   let !configCount = length (getAllConfigs features)
--   let !presentConfigs = length sh'
--   let !hdr =
--         foldr
--           (\s t -> s ++ "," ++ t)
--           ""
--           [filename, show nodeCount, show featCount, show configCount, show presentConfigs]
--   let !env = Env deep shallow filename features configCount nodeCount hdr
--   putStrLn $ "Analysis:        " ++ analysis
--   putStrLn $ "File:            " ++ filename
--   putStrLn $ "Node#:           " ++ show nodeCount
--   putStrLn $ "Features:        " ++ show features
--   putStrLn $ "Feature#:        " ++ show featCount
--   putStrLn $ "Config#:         " ++ show configCount
--   putStrLn $ "Present config#: " ++ show presentConfigs
--   return env

-- deep = Deep.analyze

-- deepMemoSt st c = runState (DeepMemo.analyze c) st

-- deepMemoMem st c = execState (DeepMemo.analyze c) st

-- deepMemo st c = evalState (DeepMemo.analyze c) st

-- data CustomArgs = CustomArgs
--   { filename :: String,
--     fileversion :: String,
--     others :: Mode
--   }

-- customParser :: Parser CustomArgs
-- customParser =
--   CustomArgs
--     <$> strOption
--       ( long "filename"
--           <> value "input file name"
--           <> metavar "STR"
--           <> help "Input CFG to process."
--       )
--     <*> strOption
--       ( long "fileversion"
--           <> value "version of the input file"
--           <> metavar "STR"
--           <> help "Version of the file to process."
--       )
--     <*> parseWith defaultConfig

-- -- Auxiliary function for removing the 'quotes' and 'slash' characters from the string stored in memory
-- -- representing the function name
-- extractFnName :: [Char] -> [Char]
-- extractFnName = filter (not . (`elem` "\\\'\""))

-- main :: IO ()
-- main = do
--   cliParams <- execParser $ describeWith customParser
--   let fileName = filename cliParams
--   let fileVersion = fileversion cliParams
--   let inputsDir = "inputs/busybox/cfgs-" <> fileVersion
--   let inputFile = inputsDir <> "/" <> fileName
--   let memoryDir = fileName <> ".memo"

--   let reconstructedMemoryIO :: IO DeepMemo.MemoryConc
--       reconstructedMemoryIO = loadMemory memoryDir
--   reconstructedMemory <- reconstructedMemoryIO

--   let changedFunctionNamesDir = "inputs/results"
--   modifiedFunctions <- readfileContents (changedFunctionNamesDir <> "/" <> fileName <> ".result")
--   let validMemory = filter (\(k, v) -> extractFnName (fst k) `notElem` modifiedFunctions) reconstructedMemory

--   runMode
--     (others cliParams)
--     [ env (setupEnv inputFile) $ \env ->
--         bgroup
--           (hdr env)
--           [ bench "deep" $ nf deep (deepCFG env),
--             bench "deepMemo" $ nf (deepMemo validMemory) (deepCFG env)
--           ]
--     ]

--   runMode
--     (others cliParams)
--     [ env (setupEnv inputFile) $ \env ->
--         let loadMemoryIO :: String -> IO DeepMemo.MemoryConc
--             loadMemoryIO = loadMemory
--          in bgroup
--               (hdr env)
--               [ bench "loadMemory" $ nfIO (loadMemoryIO memoryDir)
--               ]
--     ]

--   newMemoryEnv <- setupEnv inputFile
--   let newMemory = deepMemoMem validMemory (deepCFG newMemoryEnv)
--   storeMemory memoryDir newMemory

--   runMode
--     (others cliParams)
--     [ env (setupEnv inputFile) $ \env ->
--         let storeMemoryIO :: DeepMemo.MemoryConc -> IO ()
--             storeMemoryIO = storeMemory memoryDir
--          in bgroup
--               (hdr env)
--               [ bench "storeMemory" $ nfIO (storeMemoryIO newMemory)
--               ]
--     ]