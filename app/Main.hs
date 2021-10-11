{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances  #-}
module Main where

import Std
import RunFunctions

import Options.Generic

data Options w = Options
    { program :: w ::: String <?> "The program to verify"
    , k :: w ::: Integer <!> "10" <?> "The k to use when verifying"
    , n :: w ::: Integer <!> "2" <?> "The N to use when verifying"
    , mutate :: w ::: Bool <?> "Whether to check if the mutations fail"
    , disableOptimizations :: w ::: Bool <?> "Whether to disable the optimizations"
    } deriving (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

main :: IO ()
main = do
  options <- (unwrapRecord "Bounded verification tool"
              :: IO (Options Unwrapped))
  if mutate options
    then runMutateProgram (program options) (k options)
         (n options) (not $ disableOptimizations options)
    else verifyProgram (program options) (k options)
         (n options) (not $ disableOptimizations options)
