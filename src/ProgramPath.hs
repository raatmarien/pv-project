module ProgramPath

where

import GCLParser.GCLDatatype


type ProgramPath = [Stmt]


-- So this generates program paths now
-- Not all of them are from beginning to end.
-- And not all are generated, especially not shorter ones.
-- This should probably be fixed.
-- Wishnu says we should only include paths from beginning to end
generateProgramPaths :: Int -> Stmt -> [ProgramPath]
generateProgramPaths 0 _ = [[]]
generateProgramPaths k (Seq s1 s2)
  = map (s1:) $ generateProgramPaths (k-1) s2
generateProgramPaths k (IfThenElse e s1 s2)
  | k >= 2 = [[Assume e, s1], [Assume (OpNeg e), s2]]
  | otherwise = [[]] -- The other paths are too long
generateProgramPaths k w@(While e s)
  | k >= 2 = (map ([Assume e, s] ++) $ generateProgramPaths (k-2) w) ++
              [[Assume (OpNeg e)]]
  | otherwise = [[Assume (OpNeg e)]] -- Loop entry is too long
generateProgramPaths _ (Block _ s) = [[s]]
generateProgramPaths _ (TryCatch _ _ _)
  = undefined -- This is optional right?
generateProgramPaths _ stmt = [[stmt]]

