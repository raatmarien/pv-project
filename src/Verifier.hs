module Verifier where

import GCLParser.Parser
import GCLParser.PrettyPrint
import GCLInterpreter
import MuGCL
import WLP
import ProgramPath

-- Could be a Maybe map with counter example?
verifyProgram :: (Maybe Int) -> Int -> String -> IO Bool
verifyProgram maybeN k filename = do
  (Right prg) <- parseGCLfile filename
  putStrLn "Program to verify:\n"
  putStrLn . ppProgram2String $ prg
  putStrLn ""

  let programPaths = case maybeN of
        Just n -> generateProgramPathsWithN n k $ toStatement prg
        Nothing -> generateProgramPaths k $ toStatement prg
  
  putStrLn $ "Validating " ++ (show $ length programPaths) ++ " paths"
  
  contradictions <- mapM contradict programPaths

  let inValid = or contradictions

  if inValid
    then do putStrLn "Program is invalid!"
            let wrongPath = head $ filter fst
                             $ zip contradictions programPaths
            putStrLn "An example is:"
            print $ head wrongPaths
    else putStrLn "Program is valid."

  return $ not inValid

