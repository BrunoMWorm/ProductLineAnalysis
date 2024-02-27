-- Return Average code analysis
-- Returning the average number of goto statements per label
module GotosDeepMemo where

import Data.Maybe
import Data.Ratio
import Debug.Trace
import Language.C.Syntax.AST
import Memoization.Core.Memory
import Memoization.Core.State
import SPL
import VCFG
import Control.Exception

type MemoryConc = KeyValueArray String (Var Integer)
type StateConc = State MemoryConc

isGoto :: Var CFGNode -> Var Bool
isGoto n =
  let case0 __cntxt__ = (True ^| __cntxt__)
      split0 __dummy__ = case __dummy__ of CFGStat (CGoto _ _) -> ()
      case1 __cntxt__ = (False ^| __cntxt__)
      split1 __dummy__ = case __dummy__ of _ -> ()
   in liftedCase
        ((ast ^| ttPC) <*> n)
        ( \__dummy__ -> case __dummy__ of
            CFGStat (CGoto _ _) -> 0
            _ -> 1
        )
        [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1)]

isLabel :: Var CFGNode -> Var Bool
isLabel n =
  let case0 __cntxt__ = (True ^| __cntxt__)
      split0 __dummy__ = case __dummy__ of CFGStat (CLabel _ _ _ _) -> ()
      case1 __cntxt__ = (False ^| __cntxt__)
      split1 __dummy__ = case __dummy__ of _ -> ()
   in liftedCase
        ((ast ^| ttPC) <*> n)
        ( \__dummy__ -> case __dummy__ of
            CFGStat (CLabel _ _ _ _) -> 0
            _ -> 1
        )
        [\__cntxt__ -> (uncurry0 (case0 __cntxt__)) . (liftV split0), \__cntxt__ -> (uncurry0 (case1 __cntxt__)) . (liftV split1)]


-- A granularidade aqui só pode ser no nível CFG...
analyze :: Var CFG -> StateConc (Var Rational)
analyze cfg =
  let _ns = _nodes' cfg
      _ls = filter' isLabel _ns
      _gs = filter' isGoto _ns
      labelCount = length' _ls
      gotoCount = length' _gs
   in return $ liftedCond (((==) ^| ttPC) <*> (labelCount) <*> ((0 ^| ttPC))) (\__cntxt__ -> (0 ^| __cntxt__)) (\__cntxt__ -> ((%) ^| __cntxt__) <*> (((toInteger ^| __cntxt__) <*> (gotoCount /^ __cntxt__))) <*> (((toInteger ^| __cntxt__) <*> (labelCount /^ __cntxt__))))
