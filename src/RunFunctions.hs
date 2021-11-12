{-# OPTIONS_GHC
  -Wno-all
#-}

module RunFunctions where

import BoundedVerification (boundedVerification, boundedVerificationWithoutCounterExample, parse, verificationLeavesAmount)
import GCLUtils (mutateProgram)
import Gcl (Type (PType), PrimitiveType (PTBool), Expression (IntegerLiteral), Identifier)
import Gcl qualified
import Path qualified
import Tree (Tree (Empty))
import Tree qualified
import SymbolicExecution (Value, counterExample, verify, symbolifyPath)

import Criterion.Main
import Data.Sequence qualified as S
import Relude.Unsafe qualified as U
import Std

withoutMutations ::
  FilePath ->
  Integer ->
  Integer ->
  Bool ->
  IO (Maybe (HashMap Identifier Value))
withoutMutations file nSubstitute searchDepth prune = do
  content <- readFileUtf8 file
  let parsed = Gcl.fromParseResult $ parse $ content
      withN = Gcl.instantiateN (IntegerLiteral nSubstitute) parsed
      result = boundedVerification searchDepth prune withN
  return result

verificationBenchmark ::
  String ->
  FilePath ->
  Integer ->
  Integer ->
  Bool ->
  IO Benchmark
verificationBenchmark name file nSubstitute searchDepth prune = do
  content <- readFileUtf8 file
  let parsed = Gcl.fromParseResult $ parse $ content
      withN = Gcl.instantiateN (IntegerLiteral nSubstitute) parsed
      resultF = \n -> isJust (boundedVerification searchDepth prune n)
      benchmarkable = nf resultF withN
  return $ bench name benchmarkable

withMutations ::
  FilePath ->
  Integer ->
  Integer ->
  Bool ->
  IO [Maybe (HashMap Identifier Value)]
withMutations file nSubstitute searchDepth prune =
  (fmap . fmap) (boundedVerification searchDepth prune) $
  (fmap . fmap) (Gcl.instantiateN $ IntegerLiteral nSubstitute) $
  (fmap . fmap) Gcl.fromParseResult $
  (fmap . fmap) snd $
  fmap mutateProgram $
  fmap parse $
  readFileUtf8 file

verifyProgram :: FilePath -> Integer -> Integer -> Bool -> IO ()
verifyProgram program n k prune = do
  result <- withoutMutations program n k prune
  print result

runMutateProgram :: FilePath -> Integer -> Integer -> Bool -> IO ()
runMutateProgram program n k prune = do
  result <- withMutations program n k prune
  print result
  print $ length $ filter (/= Nothing) result
  print $ length result

getPathsAmount :: FilePath -> Integer -> Integer -> Bool -> IO Int
getPathsAmount file n k prune = do
  content <- readFileUtf8 file
  let parsed = Gcl.fromParseResult $ parse $ content
      withN = Gcl.instantiateN (IntegerLiteral n) parsed
  return $ verificationLeavesAmount k prune withN

r :: FilePath -> Integer -> Integer -> IO _
r file searchDepth nSubstitute =
  (fmap) (boundedVerificationWithoutCounterExample searchDepth True) $
  (fmap) (Gcl.instantiateN $ IntegerLiteral nSubstitute) $
  (fmap) Gcl.fromParseResult $
  -- (fmap) snd $
  -- fmap (\[_,t0,t1,t2,_,_,_,_,t3,t4,t5,_,_,_,t6,t7,t8] -> [t0,t1,t2,t3,t4,t5,t6,t7,t8]) $
  -- fmap (\[_,_,_,_,_,_,_,_,_,_,_,_,t0,t1,t2,_,_,_,_,_,_,t3,t4,_] -> [t0,t1,t2,t3,t4]) $
  -- fmap mutateProgram $
  fmap parse $
  readFileUtf8 file
