{-# OPTIONS_GHC
  -Wno-all
#-}

module RunFunctions where

import BoundedVerification (boundedVerificationWithPath, boundedVerification, parse, verificationLeavesAmount)
import GCLUtils (mutateProgram)
import Gcl (Type (PType), PrimitiveType (PTBool), Expression (IntegerLiteral), Identifier)
import Gcl qualified
import Path qualified
import Tree (Tree (Empty))
import Tree qualified
import SymbolicExecution (Value, counterExample, symbolifyPath)
import Text.Pretty.Simple (pPrint)

import Criterion.Main
import Data.Sequence qualified as S
import Std

verifyProgram ::
  FilePath ->
  Integer ->
  Integer ->
  Bool ->
  IO ()
verifyProgram file nSubstitute searchDepth prune = do
  content <- readFileUtf8 file
  let parsed = Gcl.fromParseResult $ parse $ content
      withN = Gcl.instantiateN (IntegerLiteral nSubstitute) parsed
      result = boundedVerificationWithPath searchDepth prune withN
      pathsCount = verificationLeavesAmount searchDepth False withN
  putText "paths: "
  print pathsCount
  putText "of which identified as unfeasible: "
  print (pathsCount - verificationLeavesAmount searchDepth prune withN)
  pPrint result

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

runMutateProgram :: FilePath -> Integer -> Integer -> Bool -> IO ()
runMutateProgram program n k prune = do
  result <- withMutations program n k prune
  pPrint result
  print $ length $ filter (/= Nothing) result
  print $ length result

getPathsAmount :: FilePath -> Integer -> Integer -> Bool -> IO Int
getPathsAmount file n k prune = do
  content <- readFileUtf8 file
  let parsed = Gcl.fromParseResult $ parse $ content
      withN = Gcl.instantiateN (IntegerLiteral n) parsed
  return $ verificationLeavesAmount k prune withN
