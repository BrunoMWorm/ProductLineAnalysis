module Rewrite.Common where 

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.AST.Ann
import Control.Reference ((^.), (.-))
import qualified SPL as L 
import qualified Data.Set as S 

moduleNameSPL = mkModuleName "SPL"
importSPL = mkImportDecl False False False Nothing moduleNameSPL Nothing Nothing

appendModName :: String -> ModuleName -> ModuleName
appendModName s mn = mkModuleName $ (mn ^. moduleNameString) ++ s

appOp  = mkUnqualOp "<*>"
mkVarT = mkVar (mkName "mkVarT")
mkVars = mkVar (mkName "mkVars")
tt     = mkVar (mkName "tt")
tyVar  = mkVarType $ mkName "Var"

pat2expr (VarPat n) = mkVar n 

-- | Rename module
--
updateHead :: String -> Maybe ModuleHead -> Maybe ModuleHead
updateHead suffix mh =  
    case mh of
        Just mh' -> Just $ (mhName .- (appendModName suffix)) $ mh'
        _ -> mh
    
renameModule :: String -> Module -> Module
renameModule suffix mod = (modHead .- (annMaybe .- (updateHead suffix))) mod

rewriteType :: Type -> Type
rewriteType t = case t of
    FunctionType a b -> mkFunctionType (rewriteType a) (rewriteType b)
    -- TODO: handle other cases
    _ -> mkTypeApp tyVar t

-- TODO
-- mkTypeSignature takes only one name, so a signature might map
-- to multiple declarations
rewriteTypeSig :: TypeSignature -> Decl
rewriteTypeSig (TypeSignature ns t) = 
    let n = head $ _annListElems ns
    in  mkTypeSigDecl $ mkTypeSignature n (rewriteType t)
