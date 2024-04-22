-- Case termination code analysis
-- Adapted from a similar analysis in TypeChef/CRewrite
-- Checking whether a non-void function has a return statement
module ReturnDeepMemo where

import Control.Exception
import Data.Maybe
import Debug.Trace
import Language.C.Syntax.AST
import Memoization.Core.Memory
import Memoization.Core.State
import SPL
import VCFG

type MemoryConc = KeyValueArray (String, Int) (Var Bool)
type StateConc = State MemoryConc

find :: Var Int -> [Var Int] -> Var Bool
find n _ns = case _ns of
  [] -> (False ^| ttPC)
  (h : _t) -> liftedCond (((==) ^| ttPC) <*> (h) <*> (n)) (\__cntxt__ -> (True ^| __cntxt__)) (\__cntxt__ -> find (n /^ __cntxt__) _t)

isFnRoot :: Var CFGNode -> Var Bool
isFnRoot n =
  let case0 __cntxt__ = (True ^| __cntxt__)
      split0 __dummy__ = case __dummy__ of CFGFuncRoot _ -> ()
      case1 __cntxt__ = (False ^| __cntxt__)
      split1 __dummy__ = case __dummy__ of _ -> ()
   in liftedCase
        ((ast ^| ttPC) <*> n)
        ( \__dummy__ -> case __dummy__ of
            CFGFuncRoot _ -> 0
            _ -> 1
        )
        [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1)]

isReturn :: Var CFGNode -> Var Bool
isReturn n =
  let case0 __cntxt__ = (True ^| __cntxt__)
      split0 __dummy__ = case __dummy__ of CFGStat (CReturn _ _) -> ()
      case1 __cntxt__ = (False ^| __cntxt__)
      split1 __dummy__ = case __dummy__ of _ -> ()
   in liftedCase
        ((ast ^| ttPC) <*> n)
        ( \__dummy__ -> case __dummy__ of
            CFGStat (CReturn _ _) -> 0
            _ -> 1
        )
        [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1)]

isFuncCall :: Var CFGNode -> Var Bool
isFuncCall n =
  let case0 __cntxt__ = (True ^| __cntxt__)
      split0 __dummy__ = case __dummy__ of CFGDecl _ -> ()
      case1 __cntxt__ = (True ^| __cntxt__)
      split1 __dummy__ = case __dummy__ of CFGStat (CBreak _) -> ()
      case2 __cntxt__ = (True ^| __cntxt__)
      split2 __dummy__ = case __dummy__ of CFGFuncRoot _ -> ()
      case3 __cntxt__ = (False ^| __cntxt__)
      split3 __dummy__ = case __dummy__ of _ -> ()
   in liftedCase
        ((ast ^| ttPC) <*> n)
        ( \__dummy__ -> case __dummy__ of
            CFGDecl _ -> 0
            CFGStat (CBreak _) -> 1
            CFGFuncRoot _ -> 2
            _ -> 3
        )
        [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1), \__cntxt__ -> (uncurry0 (case2 __cntxt__)) . (liftV split2), \__cntxt__ -> (uncurry0 (case3 __cntxt__)) . (liftV split3)]

followSuccessor :: Var CFG -> [Var Int] -> Var CFGNode -> Var Bool
followSuccessor cfg _visited n = liftedCond (((||) ^| ttPC) <*> ((find (_nID' n) _visited)) <*> ((isFuncCall n))) (\__cntxt__ -> (False ^| __cntxt__)) (\__cntxt__ -> liftedCond (isReturn (n /^ __cntxt__)) (\__cntxt__ -> (True ^| __cntxt__)) (\__cntxt__ -> followSuccessors (cfg /^ __cntxt__) ((_nID' (n /^ __cntxt__)) ^: _visited) (n /^ __cntxt__)))

followSuccessors :: Var CFG -> [Var Int] -> Var CFGNode -> Var Bool
followSuccessors cfg _visited n = let _ss = _succs' cfg n in foldr' (\a b -> ((||) ^| ttPC) <*> (a) <*> (b)) (False ^| ttPC) (map' (followSuccessor cfg _visited) _ss)

hasReturn :: Var CFG -> Var CFGNode -> StateConc (Var Bool)
hasReturn cfg n@(Var ns) =
  let fname = show $ _fname (fst (head ns))
      presenceCond = (snd (head ns))
   in retrieveOrRun
        (fname, _nID (fst (head ns)))
        ( \_ ->
            let follow = followSuccessors cfg [_nID' n] n
             in assert (length ns == 1) $ return follow
        )

analyze :: Var CFG -> StateConc [Var CFGNode]
analyze cfg =
  let _ns = _nodes' cfg
      fns = filter' isFnRoot _ns
   in filterM' (compositionM (not') (hasReturn cfg)) fns

filterM' :: (Var a -> StateConc (Var Bool)) -> [Var a] -> StateConc [Var a]
filterM' _ [] = return []
filterM' p (x : xs) = do
  c <- p x
  let r@(Var r') = liftedCond (c) (\pc -> x /^ pc) (\pc -> (Var []) /^ pc)
   in if null r'
        then filterM' p xs
        else do
          t <- (filterM' p xs)
          return $ r : t

compositionM :: (Var b -> Var c) -> (Var a -> StateConc (Var b)) -> (Var a -> StateConc (Var c))
compositionM f1 f2M = \a -> do
  r2 <- (f2M a)
  return $ f1 r2