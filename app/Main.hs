{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances  #-}
module Main where

import Std

import Options.Generic

data Options w = Options
    { program :: w ::: String <?> "The program to verify"
    , k :: w ::: Int <!> "10" <?> "The k to use when verifying"
    , n :: w ::: Int <!> "2" <?> "The N to use when verifying"
    , mutate :: w ::: Bool <?> "Whether to check if the mutations fail"
    , disableOptimizations :: w ::: Bool <?> "Whether to disable the optimizations"
    } deriving (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

main :: IO ()
main = do
  options <- (unwrapRecord "Bounded verification tool" :: IO (Options Unwrapped))
  print options
