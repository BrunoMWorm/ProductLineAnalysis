-------------------------------------------------------------------------------
-- SPL.hs
-- Software Product Line library
-- Ramy Shahin - July 14th 2016
-------------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}

module SPL where

import Prop
import Control.Applicative
import Control.Monad
import Control.Monad.Trans(liftIO)
import Data.List 
import Data.Maybe

type FeatureSet         = Universe
type PresenceCondition  = Prop

sat :: Prop -> Bool
sat p = True

type Val a = (Maybe a, PresenceCondition)

--instance Eq SPLOption a where
--    (==) a b = (getValue a == getValue b) && sat(getPresenceCondition a && getPresenceCondition b)

-- when lifting a product value to a product line value, we might end up with
-- different values for each product in the product line. This is why a value is
-- lifted into a set of values, each with a path condition. An important
-- inVar here is that the path conditions of those values should not depend
-- on each other, i.e. non of them logically implies the other. Violating this
-- inVar would result in redundant values (i.e. multiple values belonging
-- to the same set of products). This does not affect correctness, but severely
-- affects performance as we are now degenerating into brute force analysis
-- across all possible products.
data Var t = Var [(Val t)]

--type family Var t where
--    Var ((t :: * -> * -> * -> *) (s1 :: *) (s2 :: *) (s3 :: *))      = Var' (t (Var' s1) (Var' s2) (Var' s3))
--    Var ((t :: * -> * -> *) (s1 :: *) (s2 :: *))      = Var' (t (Var' s1) (Var' s2))
--    Var ((t :: * -> *) (s :: *))      = Var' (t (Var' s))
--    Var (t :: *)                      = Var' t

mkVar :: t -> PresenceCondition -> Var t
mkVar v pc = Var [(Just v,pc), (Nothing, neg pc)]

mkVarT :: t -> Var t
mkVarT v = mkVar v T

mkVars :: [(t,PresenceCondition)] -> Var t
mkVars vs = let nothingPC = (neg . disj) (map snd vs) 
                vs'       = map (\(v,pc) -> (Just v, pc)) vs
            in  Var ((Nothing, nothingPC) : vs')

compact :: (Eq t) => Var t -> Var t
compact (Var v) = 
    let gs = groupBy (\(v1, _) (v2, _) -> (v1 == v2)) v
    in  Var (map (\g -> let (vs, pcs) = unzip g
                        in  (head vs, disj pcs)) 

            gs)

-- lift a value
--lift :: PresenceCondition -> t -> Var' t
--lift pc x = Var' (return [(x, pc)])

--liftT :: t -> Var' t
--liftT x = lift T x


--triples :: IO (Var (a -> b)) -> IO (Var a) -> IO [(a -> b, a, PresenceCondition)]
--triples (Var fn) (Var x) = [(fn', x', c) | (fn', fnpc) <- fn, (x', xpc) <- x, let c = Conj[fnpc, xpc]]

--filteredTriples :: [(a -> b, a, PresenceCondition)] -> IO [(a -> b, a, PresenceCondition)]
--filteredTriples = filterM (\(f, x, pc) -> Prop.sat pc)
    
--compute :: IO [(a -> b, a, PresenceCondition)] -> IO (Var b)
--compute ts = do
--                ts' <- ts
--                return (Var (map (\(fn, x, pc) -> ((fn x), pc)) ts'))
                
-- apply a unary lifted function
--apply :: Var (a -> b) -> Var a -> Var b
--apply (Var fn) (Var x) = 
--    let ts = filter (\(f, x, pc) -> Prop.sat pc) [(fn', x', c) | (fn', fnpc) <- fn, (x', xpc) <- x, let c = conj[fnpc, xpc]]
--    in  Var (fmap (\(fn, x, pc) -> case (fn, x) of
--                                            (Just fn', Just x') -> (Just (fn' x'), pc)
--                                            (_, _)              -> (Nothing, pc)
--                   ) ts)

apply :: Var (a -> b) -> Var a -> Var b
apply (Var fn) (Var x) = 
    let ts = filter (\(f, x, pc) -> Prop.sat pc) [(fn', x', c) | (fn', fnpc) <- fn, (x', xpc) <- x, let c = conj[fnpc, xpc]]
    in  Var (fmap (\(fn, x, pc) -> case (fn, x) of
                                            (Just fn', Just x') -> (Just (fn' x'), pc)
                                            (_, _)              -> (Nothing, pc)
    ) ts)

--Var (do
--                                (fnVal,fnPC) <- fn
--                                (xVal,xPC)   <- x
--                                let c = Conj[fnPC,xPC]
--                                b  <- liftIO  (sat c)
--                                [(fnVal xVal, c) | b]
--                             )    

-- apply a binary lifted function
--apply2 :: Var (a -> b -> c) -> Var a -> Var b -> Var c
--apply2 fn a = apply (apply fn a)

-- apply a ternary lifted function
--apply3 :: Var (a -> b -> c -> d) -> Var a -> Var b -> Var c -> Var d
--apply3 fn a b = apply (apply2 fn a b)

-- apply a 4-arity lifted function
--apply4 :: Var (a -> b -> c -> d -> e) -> Var a -> Var b -> Var c -> Var d -> Var e
--apply4 fn a b c = apply (apply3 fn a b c)

--instance Show a => Show (Var a) where
--    show (Var (IO v)) = mapM (concat $ map (\(x,pc) -> " (" ++ show x ++ ", " ++ show pc ++ ") ")) v

-------------------------------------
-- Variability Monad??
-------------------------------------
--liftFn :: PresenceCondition -> (a -> b) -> (a -> Var b)
--liftFn pc f = (lift pc) . f

--applyFn :: (a -> Var b) -> Var a -> Var b
--applyFn fn (V x) = (map (\(x', pc) -> ((fn x'), pc)) x)

instance Show a => Show (Var a) where
    show (Var v) = "{\n" ++ (foldr (++) "" (map (\x -> (show x) ++ "\n") v)) ++ "}" 

instance Functor Var where
    fmap :: (a -> b) -> Var a -> Var b
    fmap f = apply (mkVarT f)

instance Applicative Var where
    pure  = mkVarT
    (<*>) = apply

-- lifting conditional expression
cond :: Bool -> a -> a -> a
cond p a b = if p then a else b

cond' :: Var Bool -> Var a -> Var a -> Var a
cond' = liftA3 cond

-- lifting higher-order functions
mapLifted :: Var (a -> b) -> Var [a] -> Var [b]
mapLifted = liftA2 map

filterLifted :: Var (a -> Bool) -> Var [a] -> Var [a]
filterLifted = liftA2 filter

-- lifted list


--consListLifted :: PresenceCondition -> Lifted a -> ListLifted a -> ListLifted a
--consListLifted pc x xs = consLifted x xs

--lift1 :: PresenceCondition -> (a -> b) -> (Lifted a -> Lifted b)
--lift1 pc fn = (filter (\(v,pc') -> sat pc')) . map (\(v,pc') -> (fn v, (conj [pc, pc'])))

--lift2 :: PresenceCondition -> (a -> b -> c) -> (Lifted a -> Lifted b -> Lifted c)
--lift2 pc fn = {-(filter (\(v,pc') -> sat pc')) . foldr (++) [] . -} (map (\(v,pc') -> (lift1 pc (fn v))))

-- join (Lifted-Lifted) - join 2 lifted values
--join2 :: PresenceCondition -> Var a -> Var b -> Var (a,b)
--join2 pc a b = 
--    let xProduct = [((aVal, bVal), (conj [pc, aPC, bPC])) | (aVal, aPC) <- a, (bVal, bPC) <- b]
--    in  filter (\(_, pc) -> sat pc) xProduct
    
--join3 :: PresenceCondition -> Var a -> Var b -> Var c -> Var (a,b,c)
--join3 pc a b c = 
--    let xProduct = [((aVal, bVal, cVal), (conj [pc, aPC, bPC, cPC])) | (aVal, aPC) <- a, (bVal, bPC) <- b, (cVal, cPC) <- c]
--    in  filter (\(_, pc) -> sat pc) xProduct

{-
apply2 :: PresenceCondition -> (a -> b -> c) -> Lifted a -> Lifted b -> Lifted c
apply2 cntxt fn a b = [((fn x y), pc) | ((x,y), pc) <- (join2 cntxt a b)]

apply3 :: PresenceCondition -> (a -> b -> c -> d) -> Lifted a -> Lifted b -> Lifted c -> Lifted d
apply3 cntxt fn a b c = [((fn x y z), pc) | ((x,y,z), pc) <- (join3 cntxt a b c)]
-}

-- joinUL (Unlifted-Lifted) - join an unlifted value with a lifted value
{- joinUL :: a -> Lifted b -> Lifted (a,b)
joinUL a b =
    let xProduct = [SPLOption (a, bVal) bPC | (SPLOption bVal bPC) <- b]
    in  filter (\(SPLOption _ pc) -> sat pc) xProduct -}
    
-- joinLU (Lifted-Unlifted) - join an lifed value with a unlifted value
{-joinLU :: Lifted a -> b -> Lifted (a,b)
joinLU a b =
    let xProduct = [SPLOption (aVal, b) aPC | (SPLOption aVal aPC) <- a]
    in  filter (\(SPLOption _ pc) -> sat pc) xProduct -}
    
--liftVal :: a -> Lifted a
--liftVal a = [(a,True)]

--liftFun :: (a -> b -> c) -> Lifted a -> Lifted b -> Lifted c
--liftFun fn a b = 
--    map (\(SPLOption(x,y) pc) -> SPLOption (fn x y) pc) (joinLL a b)
  
--get :: SPLContext -> SPLOption a -> Maybe a
--get cntxt v = case v of
--    SPLOption val pc -> if sat (cntxt && pc) then Just val else Nothing

liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = fmap f a <*> b <*> c <*> d

liftA5 :: Applicative f => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
liftA5 f a b c d e = fmap f a <*> b <*> c <*> d <*> e

liftV :: (a -> b) -> Var a -> Var b
liftV = liftA

liftV2 :: (a -> b -> c) -> Var a -> Var b -> Var c
liftV2 = liftA2

liftV3 :: (a -> b -> c -> d) -> Var a -> Var b -> Var c -> Var d
liftV3 = liftA3

liftV4 :: (a -> b -> c -> d -> e) -> Var a -> Var b -> Var c -> Var d -> Var e
liftV4 = liftA4

liftV5 :: (a -> b -> c -> d -> e -> f) -> Var a -> Var b -> Var c -> Var d -> Var e -> Var f
liftV5 = liftA5

cliftV  f a = compact $ (liftV f) a
cliftV2 f a b = compact $ (liftV2 f) a b
cliftV3 f a b c = compact $ (liftV3 f) a b c
cliftV4 f a b c d = compact $ (liftV4 f) a b c d
cliftV5 f a b c d e = compact $ (liftV5 f) a b c d e

--data VarOption a =
    
-- Bool operation lifting
(|==|) :: (Eq a) => Var a -> Var a -> Var Bool
(|==|) = cliftV2 (==)

{-
-- List lifting
data List' t =
    Empty'
  | Cons' t (Var' (List' t))
-}
e :: Var [a]
e = mkVarT []

(|:|) :: Var a -> Var [a] -> Var [a]
(|:|) (Var v) (Var vs) = --liftA2 (:)
    let ts = filter (\(_, _, pc) -> Prop.sat pc) [(v', vs', c) | (v', vpc) <- v, (vs', vspc) <- vs, let c = conj[vpc, vspc]]
    in  Var (map (\(v', vs', pc) -> case (v', vs') of
                                            (Just v'', Just vs'') -> (Just (v'' : vs''), pc)
                                            (Nothing, Just vs'')  -> (Just vs'', pc)
                                            (_, _)                -> (Nothing, pc)
                   ) ts)

mkVarList :: [Var t] -> Var [t]
mkVarList = foldr (|:|) e

null' :: Foldable t => Var (t a) -> Var Bool
null' = cliftV null

head' :: Eq a => Var [a] -> Var a
head' = cliftV head

tail' :: Eq a => Var [a] -> Var [a]
tail' = cliftV tail
