{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Serialization.StoreBDD (storeMemory, loadMemory) where

import Cudd.Cudd
import Cudd.File
import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.Maybe (fromJust)
import PropBDD (Prop (Prop, b), manager)
import SPL (PresenceCondition, Val, Var (Var), mkVar)
import System.Directory (doesFileExist, createDirectory, createDirectoryIfMissing)
import System.IO (IOMode (WriteMode), hGetContents, hPutStrLn, withFile)

type CUDDName = String

type FileName = String
type Path = String

storeMemory :: (Show k, Show v) => Path -> FileName -> [(k, Var v)] -> IO ()
storeMemory path fileName memory = do
  createDirectoryIfMissing True path
  let valuesDir = valuesDirectory path
  createDirectoryIfMissing True valuesDir
  let bddsDir = bddsDirectory path
  createDirectoryIfMissing True bddsDir
  let valuesWithoutPCs = map (\(k, Var vs) -> (k, fmap fst vs)) memory
  storeValuesWithoutPCs valuesDir fileName valuesWithoutPCs
  serializeBDDs bddsDir memory

loadMemory :: (Read k, Ord k, Show k, Read v) => Path -> FileName -> IO [(k, Var v)]
loadMemory path fileName = do
  fileExists <- doesFileExist (valuesDirectory path <> fileName)
  if fileExists
    then do
      valuesWithoutPCs <- readValuesWithoutPCs path fileName
      reconstructMemoryWithPCs path valuesWithoutPCs
    else return []

cuddManager :: DDManager
cuddManager = manager

valuesDirectory :: Path -> Path
valuesDirectory path = path <> "values/"

bddsDirectory :: Path -> Path
bddsDirectory path = path <> "bdds/"

storeValuesWithoutPCs :: (Show k, Show v) => Path -> FileName -> [(k, [v])] -> IO ()
storeValuesWithoutPCs valuesPath filename keyValues = withFile (valuesPath <> filename) WriteMode $ \handle ->
  mapM_ (\(key, values) -> mapM_ (\(idx, value) -> hPutStrLn handle (formatLine idx key value)) $ zip [0 ..] values) keyValues
  where
    formatLine :: (Show k, Show v) => Int -> k -> v -> String
    formatLine idx key value = show idx ++ " " ++ show key ++ " " ++ show value

readValuesWithoutPCs :: (Read k, Eq k, Ord k, Read v) => Path -> FileName -> IO [(k, [v])]
readValuesWithoutPCs path filename = do
  contents <- readFile (valuesDirectory path <> filename)
  let parsedLines = map parseLine . lines $ contents
  let groupedByKey = groupBy ((==) `on` fst) . sortOn fst $ parsedLines
  return $ map (\group -> (fst $ head group, map snd group)) groupedByKey
  where
    parseLine :: (Read k, Read v) => String -> (k, v)
    parseLine line =
      let (_ : strKey : valueParts) = words line
          key = read strKey
          value = read (unwords valueParts)
       in (key, value)

serializeBDDs :: Show k => Path -> [(k, Var v)] -> IO ()
serializeBDDs bddsPath = mapM_ (\(k, Var v) -> mapM_ (\(idx, (res, pc)) -> storePresenceCondition pc (show idx <> show k) (show idx <> show k)) (zip [0 ..] v))
  where
    storePresenceCondition :: PresenceCondition -> CUDDName -> FileName -> IO Bool
    storePresenceCondition pc nodeName filename = cuddBddStore cuddManager nodeName (b pc) [] DddmpModeText DddmpVarids (bddsPath <> filename)

reconstructMemoryWithPCs :: (Show k) => Path -> [(k, [v])] -> IO [(k, Var v)]
reconstructMemoryWithPCs path = mapM (\(k, vs) -> fmap ((\var -> (k, var)) . Var) (mapM (\(idx, v) -> processValue idx k v) (zip [0 ..] vs)))
  where
    processValue :: Show k => Int -> k -> v -> IO (Val v)
    processValue idx k v = do
      ddNode <- loadBDD (show idx <> show k)
      return (v, Prop $ fromJust ddNode)
    loadBDD :: String -> IO (Maybe DDNode)
    loadBDD filePath = cuddBddLoad manager DddmpVarMatchids [] [] DddmpModeText (bddsDirectory path <> filePath)