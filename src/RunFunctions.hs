module RunFunctions where

import BoundedVerification (boundedVerification, parse)
import GCLUtils (mutateProgram)
import Gcl (Type (PType), PrimitiveType (PTBool), Expression (IntegerLiteral), Identifier)
import Gcl qualified
import Path qualified
import Tree (Tree (Empty))
import Tree qualified
import SymbolicExecution (Value, counterExample, symbolifyPath)

import Criterion.Main
import Data.Sequence qualified as S
import Std

withoutMutations ::
  FilePath ->
  Integer ->
  Integer ->
  Bool ->
  IO (Maybe (HashMap Identifier Value))
withoutMutations file nSubstitute searchDepth prune = do
  content <- readFileBS file
  let parsed = Gcl.fromParseResult $ parse $ decodeUtf8 content
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
  content <- readFileBS file
  let parsed = Gcl.fromParseResult $ parse $ decodeUtf8 content
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
  fmap decodeUtf8 $
  readFileBS file

verifyProgram :: FilePath -> Integer -> Integer -> Bool -> IO ()
verifyProgram program n k prune = do
  result <- withoutMutations program n k prune
  print result

runMutateProgram :: FilePath -> Integer -> Integer -> Bool -> IO ()
runMutateProgram program n k prune = do
  result <- withMutations program n k prune
  print result
