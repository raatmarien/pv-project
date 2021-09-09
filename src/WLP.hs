module WLP

where

--import Control.Monad
import Control.Monad.Trans.State

import Data.SBV
import qualified Data.Map as Map
import qualified Data.Set as Set

import ProgramPath
import GCLParser.GCLDatatype


type Env = Map.Map String SInteger
type EBool = Env -> SBool
type EInt = Env -> SInteger
type Free = Set.Set String

{-
return2 :: (Monad m, Monad n) => a -> m (n a)
return2 = return . return


fmap2 :: (Monad m, Monad n) => (a -> b) -> m (n a) -> m (n b)
fmap2 = (<$>) . (<$>)


ap2 :: (Monad m, Monad n) => m (n (a -> b)) -> m (n a) -> m (n b)
ap2 = ap . (ap <$>)
-}

wlpPath :: ProgramPath -> (Free, EBool)
wlpPath p = (fv, b)
    where (fs, fv) = runState (mapM wlp p) Set.empty
          b = foldr ($) (const sTrue) fs


wlp :: Stmt -> State Free (EBool -> EBool)
wlp Skip = return id
wlp (Assert x) = state $ \fv -> let (sx, fv') = symbolizeB x fv in (\post e -> sx e .&& post e, fv')
wlp (Assume x) = state $ \fv -> let (sx, fv') = symbolizeB x fv in (\post e -> sx e .=> post e, fv')
wlp (Assign v x) = state $ \fv -> let (sx, fv') = symbolizeE x fv in (\post e -> post (Map.insert v (sx e) e), fv')


symbolizeB :: Expr -> Free -> (EBool, Free)
symbolizeB (LitB x) fv = (const $ literal x, fv)
symbolizeB (OpNeg x) fv = let (sx, fv') = symbolizeB x fv in (sNot . sx, fv')
symbolizeB (BinopExpr LessThan x y) fv = let (sx, fv') = symbolizeE x fv in let (sy, fv'') = symbolizeE y fv' in (\e -> sx e .< sy e, fv'')
symbolizeB (BinopExpr Equal x y) fv = let (sx, fv') = symbolizeE x fv in let (sy, fv'') = symbolizeE y fv' in (\e -> sx e .== sy e, fv'')
symbolizeB (BinopExpr GreaterThanEqual x y) fv = let (sx, fv') = symbolizeE x fv in let (sy, fv'') = symbolizeE y fv' in (\e -> sx e .>= sy e, fv'')


symbolizeE :: Expr -> Free -> (EInt, Free)
symbolizeE (LitI x) fv = (const $ literal $ fromIntegral x, fv)
symbolizeE (Var v) fv = ((Map.! v), Set.insert v fv)


test :: IO ()
test = do
    --let path = [Assume (opLessThan (Var "x") (Var "y")), Assume (opLessThan (Var "y") (LitI 3)), Assert (opLessThan (Var "x") (LitI 1))]
    --let rhoE4 = [Assume (opLessThan (LitI 1) (Var "x")), Assume (OpNeg $ opLessThan (LitI 0) (Var "x")), Assign "y" (Var "x"), Assert (opEqual (Var "y") (LitI 1))]
    let notTest = [Assume (opLessThan (LitI 1) (Var "x")), Assume (OpNeg $ opLessThan (LitI 2) (Var "x")), Assign "y" (Var "x"), Assert (opEqual (Var "y") (LitI 2))]

    let (fv, wpe) = wlpPath notTest
    let env = sequence $ Map.fromList $ map (\v -> (v, sInteger v)) $ Set.toAscList fv

    let wp = wpe <$> env

    prove wp >>= print