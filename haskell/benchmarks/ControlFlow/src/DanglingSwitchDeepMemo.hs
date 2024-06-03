-- Dangling switch code analysis
-- Adapted from a similar analysis in TypeChef/CRewrite
-- Details at // https://www.securecoding.cert.org/confluence/display/seccode/MSC17-C.+Finish+every+set+of+statements+associated+with+a+case+label+with+a+break+statement
module DanglingSwitchDeepMemo where

import Control.Exception
import Data.Maybe
import Debug.Trace
import Language.C.Syntax.AST
import Memoization.Core.Memory
import Memoization.Core.State
import SPL
import VCFG

--import qualified Data.MultiMap as M

type MemoryConc = KeyValueArray (String, Int) (Var Bool)
type StateConc = State MemoryConc

find :: Var Int -> [Var Int] -> Var Bool
find n _ns = case _ns of
  [] -> (False ^| ttPC)
  (h : _t) -> liftedCond (((==) ^| ttPC) <*> (h) <*> (n)) (\__cntxt__ -> (True ^| __cntxt__)) (\__cntxt__ -> find (n /^ __cntxt__) _t)

isSwitch :: Var CFGNode -> Var Bool
isSwitch n =
  let case0 __cntxt__ = (True ^| __cntxt__)
      split0 __dummy__ = case __dummy__ of CFGStat (CSwitch _ _ _) -> ()
      case1 __cntxt__ = (False ^| __cntxt__)
      split1 __dummy__ = case __dummy__ of _ -> ()
   in liftedCase
        ((ast ^| ttPC) <*> n)
        ( \__dummy__ -> case __dummy__ of
            CFGStat (CSwitch _ _ _) -> 0
            _ -> 1
        )
        [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1)]

isCase :: Var CFGNode -> Var Bool
isCase n =
  let case0 __cntxt__ = (True ^| __cntxt__)
      split0 __dummy__ = case __dummy__ of CFGStat (CCase _ _ _) -> ()
      case1 __cntxt__ = (True ^| __cntxt__)
      split1 __dummy__ = case __dummy__ of CFGStat (CDefault _ _) -> ()
      case2 __cntxt__ = (False ^| __cntxt__)
      split2 __dummy__ = case __dummy__ of _ -> ()
   in liftedCase
        ((ast ^| ttPC) <*> n)
        ( \__dummy__ -> case __dummy__ of
            CFGStat (CCase _ _ _) -> 0
            CFGStat (CDefault _ _) -> 1
            _ -> 2
        )
        [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1), \__cntxt__ -> (uncurry0 (case2 __cntxt__)) . (liftV split2)]

isDecl :: Var CFGNode -> Var Bool
isDecl n =
  let case0 __cntxt__ = (True ^| __cntxt__)
      split0 __dummy__ = case __dummy__ of CFGVarDecl _ -> ()
      case1 __cntxt__ = (True ^| __cntxt__)
      split1 __dummy__ = case __dummy__ of CFGDecl _ -> ()
      case2 __cntxt__ = (False ^| __cntxt__)
      split2 __dummy__ = case __dummy__ of _ -> ()
   in liftedCase
        ((ast ^| ttPC) <*> n)
        ( \__dummy__ -> case __dummy__ of
            CFGVarDecl _ -> 0
            CFGDecl _ -> 1
            _ -> 2
        )
        [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1), \__cntxt__ -> (uncurry0 (case2 __cntxt__)) . (liftV split2)]

isFuncCall :: Var CFGNode -> Var Bool
isFuncCall n =
  let case0 __cntxt__ = (True ^| __cntxt__)
      split0 __dummy__ = case __dummy__ of CFGDecl _ -> ()
      case1 __cntxt__ = (True ^| __cntxt__)
      split1 __dummy__ = case __dummy__ of CFGFunc _ -> ()
      case2 __cntxt__ = (False ^| __cntxt__)
      split2 __dummy__ = case __dummy__ of _ -> ()
   in liftedCase
        ((ast ^| ttPC) <*> n)
        ( \__dummy__ -> case __dummy__ of
            CFGDecl _ -> 0
            CFGFunc _ -> 1
            _ -> 2
        )
        [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1), \__cntxt__ -> (uncurry0 (case2 __cntxt__)) . (liftV split2)]

followSuccessor :: Var CFG -> [Var Int] -> Var CFGNode -> Var Bool
followSuccessor cfg _visited n = liftedCond (((||) ^| ttPC) <*> ((find (_nID' n) _visited)) <*> (((||) ^| ttPC) <*> ((isCase n)) <*> ((isFuncCall n)))) (\__cntxt__ -> (True ^| __cntxt__)) (\__cntxt__ -> liftedCond (isDecl (n /^ __cntxt__)) (\__cntxt__ -> followSuccessors (cfg /^ __cntxt__) ((_nID' (n /^ __cntxt__)) ^: _visited) (_succs' (cfg /^ __cntxt__) (n /^ __cntxt__))) (\__cntxt__ -> (False ^| __cntxt__)))

followSuccessors :: Var CFG -> [Var Int] -> [Var CFGNode] -> Var Bool
followSuccessors cfg visited ns = foldr' (\a b -> ((&&) ^| ttPC) <*> (a) <*> (b)) (True ^| ttPC) (map' (followSuccessor cfg visited) ns)

danglingSwitch :: Var CFG -> Var CFGNode -> StateConc (Var Bool)
danglingSwitch cfg n@(Var ns) =
  let fname = show $ _fname (fst (head ns))
      presenceCond = (snd (head ns))
   in retrieveOrRun
        (fname, hash (show presenceCond))
        ( \_ ->
            let ss = filter' (not' ^. isCase) (_succs' cfg n)
                follow = followSuccessors cfg [] ss
             in assert (length ns == 1) $ return follow
        )

analyze :: Var CFG -> StateConc [Var CFGNode]
analyze cfg =
  let _ns = _nodes' cfg
      switches = filter' isSwitch _ns
   in filterM' (compositionM not' (danglingSwitch cfg)) switches

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