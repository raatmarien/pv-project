module ProgramPath

where

import qualified Data.Map as M

import GCLParser.GCLDatatype


data BasicStmt
  = BAssert Expr
  | BAssume Expr
  | BAssign String Expr
  | BAAssign String Expr Expr
  | BDrefAssign String Expr
  | BVarDecl String Type
  deriving (Show, Eq)

type ProgramPath = [BasicStmt]

type PathEnv = (M.Map String String, Int)

toStatement :: Program -> Stmt
toStatement prg = Block ((input prg) ++ (output prg)) $ stmt prg

-- This function generates all paths of up to the given length for a
-- statement. Only paths that go all the way from start to end are
-- included. When no paths of the length reach the end, this may return
-- an empty list.
-- VarDecl's aren't statements in the sense of our path length, so we
-- don't count them for the max length.
-- Later we might want to change this to generate program trees, that
-- we can then prune with feasibility checking.
generateProgramPaths :: Int -> Stmt -> [ProgramPath]
generateProgramPaths = generateProgramPaths' (M.empty, 0)

generateProgramPaths' :: PathEnv -> Int -> Stmt -> [ProgramPath]
generateProgramPaths' _ _ Skip = [[]]
generateProgramPaths' env k (Block decls s)
  = let f (xs, (env', i)) (VarDeclaration n t)
          = (xs ++ [BVarDecl n' t],
             (M.insert n n' env', i + 1))
          where n' = n ++ "_" ++ show i
        (basicDecls, env'') = foldl f ([], env) decls
    in map (basicDecls ++) $ generateProgramPaths' env'' k s
generateProgramPaths' env k (Seq s1 s2)
  = [ p1 ++ p2
    | p1 <- generateProgramPaths' env k s1
    , p2 <- generateProgramPaths' env (k - (length p1)) s2]
generateProgramPaths' _ 0 _ = [] -- Oops, we can't reach the end
generateProgramPaths' env k (IfThenElse e s1 s2)
  = let e' = replaceE env e
        ifCase = map ((BAssume e'):) $ generateProgramPaths' env (k-1) s1
        elseCase = map ((BAssume $ OpNeg $ e'):)
                   $ generateProgramPaths' env (k-1) s2
    in ifCase ++ elseCase
generateProgramPaths' env k w@(While e s)
  = let e' = replaceE env e
        loopCase = map ((BAssume e'):)
                   $ generateProgramPaths' env (k-1) (Seq s w)
        endCase  = [[BAssume $ OpNeg e']]
    in loopCase ++ endCase
generateProgramPaths' env _ s = [[toBasicStmt env s]]

replaceE :: PathEnv -> Expr -> Expr
replaceE env = mapExpr (\s -> M.findWithDefault s s (fst env))

toBasicStmt :: PathEnv -> Stmt -> BasicStmt
toBasicStmt env (Assert e) = BAssert $ replaceE env e
toBasicStmt env (Assume e) = BAssume $ replaceE env e
toBasicStmt env (Assign s e) = BAssign (M.findWithDefault s s (fst env)) $ replaceE env e
toBasicStmt env (AAssign s e1 e2)
  = BAAssign (M.findWithDefault s s (fst env)) (replaceE env e1) (replaceE env e2)
toBasicStmt env (DrefAssign s e)
  = BDrefAssign (M.findWithDefault s s (fst env)) $ replaceE env e
toBasicStmt _ s = error $ "The statement '" ++ show s ++ "' is not a basic statement"
