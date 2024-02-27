{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use >>" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Memoization.Core.Memory where

import Memoization.Core.State (State (State, runState))
import Debug.Trace

class KeyMemory k v m where
  mlookup :: k -> State m (Maybe v)
  mupdate :: k -> v -> State m ()

type KeyValueArray k v = [(k, v)]

instance Eq k => (KeyMemory k v) (KeyValueArray k v) where
  mlookup :: k -> State (KeyValueArray k v) (Maybe v)
  mlookup a = State (\s -> (lookup a s, s))
  mupdate :: k -> v -> State (KeyValueArray k v) ()
  mupdate a v =
    State
      ( \s ->
          ((), (a, v) : s)
      )

retrieveOrRun :: (KeyMemory k v m) => k -> (() -> State m v) -> State m v
retrieveOrRun x t =
  mlookup x
    >>= ( \case
            Just v -> return v
            Nothing -> t () >>= (\v -> mupdate x v >>= \_ -> return v)
        )
