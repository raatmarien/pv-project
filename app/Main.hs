module Main where

import Std
import RunFunctions

import qualified System.Environment as S
import Criterion.Main  
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
  b <- verificationBenchmark (program options) (n options)
          (k options) (not $ disableOptimizations options)
  let newMain = defaultMainWith defaultConfig [
        bgroup "verification" [b]
        ]
  S.withArgs [] newMain

main :: IO ()
main = do
  options <- (unwrapRecord "Bounded verification tool"
              :: IO (Options Unwrapped))
  if benchmark options
    then benchmarkProgram options
    else runProgram options

