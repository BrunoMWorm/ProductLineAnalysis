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
import System.Directory (doesFileExist)
import System.IO (IOMode (WriteMode), hGetContents, hPutStrLn, withFile)

type CUDDName = String

type FileName = String

storeMemory :: (Show k, Show v) => FileName -> [(k, Var v)] -> IO ()
storeMemory fileName memory = do
  let valuesWithoutPCs = map (\(k, Var vs) -> (k, fmap fst vs)) memory
  storeValuesWithoutPCs fileName valuesWithoutPCs
  serializeBDDs memory

loadMemory :: (Read k, Ord k, Show k, Read v) => FileName -> IO [(k, Var v)]
loadMemory fileName = do
  fileExists <- doesFileExist (valuesDirectory <> fileName)
  if fileExists
    then do
      valuesWithoutPCs <- readValuesWithoutPCs fileName
      reconstructMemoryWithPCs valuesWithoutPCs
    else return []

cuddManager :: DDManager
cuddManager = manager

valuesDirectory :: [Char]
valuesDirectory = "values/"

bddDirectory :: [Char]
bddDirectory = "bdds/"

storeValuesWithoutPCs :: (Show k, Show v) => FileName -> [(k, [v])] -> IO ()
storeValuesWithoutPCs filename keyValues = withFile (valuesDirectory <> filename) WriteMode $ \handle ->
  mapM_ (\(key, values) -> mapM_ (\(idx, value) -> hPutStrLn handle (formatLine idx key value)) $ zip [0 ..] values) keyValues
  where
    formatLine :: (Show k, Show v) => Int -> k -> v -> String
    formatLine idx key value = show idx ++ " " ++ show key ++ " " ++ show value

readValuesWithoutPCs :: (Read k, Eq k, Ord k, Read v) => FileName -> IO [(k, [v])]
readValuesWithoutPCs filename = do
  contents <- readFile (valuesDirectory <> filename)
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

serializeBDDs :: Show k => [(k, Var v)] -> IO ()
serializeBDDs = mapM_ (\(k, Var v) -> mapM_ (\(idx, (res, pc)) -> storePresenceCondition pc (show idx <> show k) (show idx <> show k)) (zip [0 ..] v))
  where
    storePresenceCondition :: PresenceCondition -> CUDDName -> FileName -> IO Bool
    storePresenceCondition pc nodeName filename = cuddBddStore cuddManager nodeName (b pc) [] DddmpModeText DddmpVarids (bddDirectory <> filename)

reconstructMemoryWithPCs :: (Show k) => [(k, [v])] -> IO [(k, Var v)]
reconstructMemoryWithPCs = mapM (\(k, vs) -> fmap ((\var -> (k, var)) . Var) (mapM (\(idx, v) -> processValue idx k v) (zip [0 ..] vs)))
  where
    processValue :: Show k => Int -> k -> v -> IO (Val v)
    processValue idx k v = do
      ddNode <- loadBDD (show idx <> show k)
      return (v, Prop $ fromJust ddNode)
    loadBDD :: String -> IO (Maybe DDNode)
    loadBDD filePath = cuddBddLoad manager DddmpVarMatchids [] [] DddmpModeText (bddDirectory <> filePath)