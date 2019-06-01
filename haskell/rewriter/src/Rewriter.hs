module Rewriter where

-- AST types: https://github.com/haskell-tools/haskell-tools/tree/master/src/ast/Language/Haskell/Tools/AST/Representation
-- AST functional constructors: 
import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor
    
import Control.Reference ((^.), (!~), (.-), (&))
import PrelNames (dollarName)
import SrcLoc (RealSrcSpan)

run :: String -> String -> IO ()
run = tryRefactor (localRefactoring . rewrite)
    
rewrite :: RealSrcSpan -> LocalRefactoring
rewrite sp = return . (nodesContained sp .- liftDecl)
                       -- .- lift) -- & annList .- liftDeclaration)

--tyVar = mkNamedWildcardType $ mkName "Var"
tyVar = mkVarType $ mkName "Var"

liftType t = mkTypeApp tyVar t

liftDecl :: Type -> Type
liftDecl t@(FunctionType _ _) = t
liftDecl t = liftType t

--liftBind :: ValueBind -> ValueBind
--liftBind (SimpleBind pat rhs locals) = mkSimpleBind pat rhs (locals ^. annMaybe)

--liftOp :: Operator -> Expr
--liftOp op = mkApp (mkUnqualOp' (mkName "mkVarT")) op

appOp =  mkUnqualOp "<*>"

mkVarT = mkVar (mkName "mkVarT")
apply  = mkVar (mkName "apply")
apply2 = mkVar (mkName "apply2")

liftOp (NormalOp o) = mkApp apply2 (mkParen (mkApp mkVarT (mkVar (mkParenName o))))

lift :: Expr -> Expr
--liftExpr (ModuleHead name pragmas exports) = mkModuleHead (name ++ "\'") pragmas exports

lift (App fun arg) = mkInfixApp fun appOp arg

lift (InfixApp arg1 op arg2) = 
    mkApp (mkApp (liftOp op) arg1) arg2

lift (Lit l) = mkParen $ mkApp mkVarT (mkLit l)

lift e = e 