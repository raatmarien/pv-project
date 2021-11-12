module Main where

import Std
import RunFunctions

import qualified System.Environment as S
import Criterion.Main  
import Criterion.Types
import Options.Generic
import qualified Data.List as L

data Options w = Options
    { program :: w ::: String <?> "The program to verify"
    , k :: w ::: Integer <!> "10" <?> "The k to use when verifying"
    , n :: w ::: Integer <!> "2" <?> "The N to use when verifying"
    , mutate :: w ::: Bool <?> "Whether to check if the mutations fail"
    , disableOptimizations :: w ::: Bool <?> "Whether to disable the optimizations"
    , benchmark :: w ::: Bool <?> "Whether to benchmark. If enabled, we don't mutate"
    } deriving (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

runProgram :: (Options Unwrapped) -> IO ()
runProgram options =
  if mutate options
     then runMutateProgram (program options) (n options)
          (k options) (not $ disableOptimizations options)
     else verifyProgram (program options) (n options)
          (k options) (not $ disableOptimizations options)

benchmarkProgram :: (Options Unwrapped) -> IO ()
benchmarkProgram options = do
  let createVerificationBenchmark (n', k', prune)
        = verificationBenchmark name (program options) n' k' prune
        where name = (show n') ++ "," ++ (show k') ++ ","
                     ++ (if prune then "1" else "0")
      getPathsInspected (n', k', prune) = do
        am <- getPathsAmount (program options) n' k' prune
        return [show n', show k', show prune, show am]
      benchOptions = [(n', k', prune) | n' <- [2..(n options)]
                                      , k' <- [10, 20..(k options)]
                                      , prune <- [True, False]]
      createCsv :: [[String]] -> String
      createCsv = L.unlines . map (L.intercalate ",")
  benchmarks <- mapM createVerificationBenchmark benchOptions
  pathsInspected <- mapM getPathsInspected benchOptions
  let csvText = createCsv $ ["n", "k", "prune", "paths"]:pathsInspected
  writeFileUtf8 "paths-inspected.csv" csvText
  let config = defaultConfig { csvFile = Just "benchmarkoutput.csv" }
      newMain = defaultMainWith config [
        bgroup "verification" benchmarks
        ]
  S.withArgs [] newMain

main :: IO ()
main = do
  options <- (unwrapRecord "Bounded verification tool"
              :: IO (Options Unwrapped))
  if benchmark options
    then benchmarkProgram options
    else runProgram options

