{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TupleSections #-}

module WLP

where

import Control.Monad
import Control.Monad.Trans.State

import Data.Functor
import Data.Bifunctor
import Data.SBV
import qualified Data.Map as Map
import qualified Data.Set as Set

import ProgramPath
import GCLParser.GCLDatatype


type Env = Map.Map String SInteger
type Free = Set.Set String


return2 :: (Monad m, Monad n) => a -> m (n a)
return2 = return . return


fmap2 :: (Monad m, Monad n) => (a -> b) -> m (n a) -> m (n b)
fmap2 = (<$>) . (<$>)


ap2 :: (Monad m, Monad n) => m (n (a -> b)) -> m (n a) -> m (n b)
ap2 = ap . (ap <$>)


wlpPath :: ProgramPath -> ([State (SBool, Env) ()], Free)
wlpPath p = runState (mapM wlp p) Set.empty


twist :: (SBool -> SBool -> SBool) -> State Env SBool -> State (SBool, Env) ()
twist op s = modify $ \(rhs, env) -> first (`op` rhs) (runState s env)


wlp :: Stmt -> State Free (State (SBool, Env) ())
wlp Skip = return2 ()
wlp (Assert x) = twist (.&&) <$> symbolizeB x
wlp (Assume x) = twist (.=>) <$> symbolizeB x


symbolizeB :: Expr -> State Free (State Env SBool)
symbolizeB (LitB x) = return2 $ literal x
symbolizeB (BinopExpr LessThan x y) = fmap2 (.<) (symbolizeE x) `ap2` symbolizeE y
symbolizeB (BinopExpr Equal x y) = fmap2 (.==) (symbolizeE x) `ap2` symbolizeE y


symbolizeE :: Expr -> State Free (State Env SInteger)
symbolizeE (LitI x) = return2 $ fromIntegral x
symbolizeE (Var v) = modify (Set.insert v) >> return (get <&> (Map.! v))


test :: IO ()
test = do
    let path = [Assume (opLessThan (Var "x") (Var "y")), Assume (opLessThan (Var "y") (LitI 3)), Assert (opLessThan (Var "x") (LitI 1))]

    let (wpS, fv) = wlpPath path
    let env = sequence $ Map.fromList $ map (\v -> (v, sInteger v)) $ Set.toAscList fv

    let wp = fst . (\e -> foldr execState (sTrue, e) wpS) <$> env

    prove wp >>= print