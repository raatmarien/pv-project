module Verifier where

import GCLParser.Parser
import GCLParser.PrettyPrint
import GCLInterpreter
import MuGCL
import WLP
import ProgramPath

-- Could be a Maybe map with counter example?
verifyProgram :: Int -> String -> IO Bool
verifyProgram k filename = do
  (Right prg) <- parseGCLfile filename
  putStrLn "Program to verify:\n"
  putStrLn . ppProgram2String $ prg
  putStrLn ""

  let programPaths = generateProgramPaths k $ toStatement prg
  
  putStrLn $ "Validating " ++ (show $ length programPaths) ++ " paths"
  
  contradictions <- mapM contradict programPaths

  let inValid = or contradictions

  if inValid
    then do putStrLn "Program is invalid!"
            let wrongPaths = filter fst
                             $ zip contradictions programPaths
            putStrLn $ (show $ length wrongPaths) ++ " paths were invalid"
            putStrLn "An example is:"
            print $ head wrongPaths
    else putStrLn "Program is valid."

  return $ not inValid