module Main where

import Std
import RunFunctions

import qualified System.Environment as S
import Criterion.Main  
import Criterion.Types
import Options.Generic

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
      benchOptions = [(n', k', prune) | n' <- [2..(n options)]
                                      , k' <- [10, 20..(k options)]
                                      , prune <- [True, False]]
  benchmarks <- mapM createVerificationBenchmark benchOptions
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

