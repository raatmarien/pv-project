module ProgramPath

where

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


-- This function generates all paths of up to the given length for a
-- statement. Only paths that go all the way from start to end are
-- included. When no paths of the length reach the end, this may return
-- an empty list.
generateProgramPaths :: Int -> Stmt -> [ProgramPath]
generateProgramPaths _ Skip = [[]]
generateProgramPaths k (Block _ s) = generateProgramPaths k s
generateProgramPaths k (Seq s1 s2)
  = [ p1 ++ p2
    | p1 <- generateProgramPaths k s1
    , p2 <- generateProgramPaths (k - (length p1)) s2]
generateProgramPaths 0 _ = [] -- Oops, we can't reach the end
generateProgramPaths k (IfThenElse e s1 s2)
  = let ifCase = map ((BAssume e):) $ generateProgramPaths (k-1) s1
        elseCase = map ((BAssume $ OpNeg e):)
                   $ generateProgramPaths (k-1) s2
    in ifCase ++ elseCase
generateProgramPaths k w@(While e s)
  = let loopCase = map ((BAssume e):)
                   $ generateProgramPaths (k-1) (Seq s w)
        endCase  = [[BAssume $ OpNeg e]]
    in loopCase ++ endCase
generateProgramPaths _ stmt = [[toBasicStmt stmt]]

toBasicStmt :: Stmt -> BasicStmt
toBasicStmt (Assert e) = BAssert e
toBasicStmt (Assume e) = BAssume e
toBasicStmt (Assign s e) = BAssign s e
toBasicStmt (AAssign s e1 e2) = BAAssign s e1 e2
toBasicStmt (DrefAssign s e) = BDrefAssign s e
toBasicStmt s = error $ "The statement '" ++ show s ++ "' is not a basic statement"
