{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Memoization.Core.State ((<.>), runState, State (State), Functor, Applicative, Monad, execState, evalState) where

newtype State m a = State {runState :: m -> (a, m)}

instance Functor (State s') where
  fmap :: (a -> b) -> State s' a -> State s' b
  fmap f am = State $ \s ->
    let (a, s') = runState am s
     in (f a, s')

instance Applicative (State s') where
  pure :: a -> State s' a
  pure x = State (x,)
  (<*>) :: State s' (a -> b) -> State s' a -> State s' b
  fm <*> am = State $ \s ->
    let (f, s') = runState fm s
        (a, s'') = runState am s'
     in (f a, s'')

instance Monad (State s') where
  (>>=) :: State s' a -> (a -> State s' b) -> State s' b
  h >>= f = State $ \s ->
    let (a, newState) = runState h s
        g = f a
     in runState g newState

(<.>) :: State m (a -> State m b) -> State m a -> State m b
fm <.> xm = fm >>= (xm >>=)

evalState :: State s a -> s -> a
evalState act = fst . runState act

execState :: State s a -> s -> s
execState act = snd . runState act

infixl 9 <.>