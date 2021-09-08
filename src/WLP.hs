{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module WLP

where


import Data.SBV
import qualified Data.Map as Map

import ProgramPath
import GCLParser.GCLDatatype


type Env = Map.Map String SInteger


wlpPath :: ProgramPath -> Env -> (SBool, Env)
wlpPath p e = foldr (uncurry . wlp) (sTrue, e) p


wlp :: Stmt -> SBool -> Env -> (SBool, Env)
wlp Skip r e = (r, e)
wlp (Assert x) r e = (cond .&& r, e')
    where (cond, e') = symbolizeB x e
wlp (Assume x) r e = (cond .=> r, e')
    where (cond, e') = symbolizeB x e


symbolizeB :: Expr -> Env -> (SBool, Env)
symbolizeB (LitB x) e = (literal x, e)
symbolizeB (BinopExpr LessThan x y) e = (lhs .< rhs, e'')
    where (lhs, e') = symbolizeE x e
          (rhs, e'') = symbolizeE y e'
symbolizeB (BinopExpr Equal x y) e = (lhs .== rhs, e'')
    where (lhs, e') = symbolizeE x e
          (rhs, e'') = symbolizeE y e'


-- change to
-- symbolizeE :: Expr -> ([String], Env -> (SInteger, Env))
-- and pass up the free variables through [String]
-- and automatically fill these in in the Map.fromList for env
symbolizeE :: Expr -> Env -> (SInteger, Env)
symbolizeE (LitI x) e = (literal $ fromIntegral x, e)
symbolizeE (Var v) e = (e Map.! v, e)


test :: IO ()
test = do
    let path = [Assume (opLessThan (LitI 1) (Var "x")), Assert (opLessThan (LitI 1) (Var "x"))]

    let env = Map.fromList [("x", sInteger "x")]:: Map.Map String (Symbolic SInteger)
    let envS = sequence env:: Symbolic (Map.Map String SInteger)
        
    let wp = fst . wlpPath path <$> envS:: Symbolic SBool

    prove wp >>= print