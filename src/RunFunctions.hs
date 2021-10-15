module RunFunctions where

import BoundedVerification (boundedVerification, mergeResults, parse)
import GCLUtils (mutateProgram)
import Gcl (Type (PType), PrimitiveType (PTBool), Expression (IntegerLiteral), Identifier)
import Gcl qualified
import Path qualified
import Tree (Tree (Empty))
import Tree qualified
import SymbolicExecution (Value, Certainty (Certain), counterExample, symbolifyPath)

import Data.Sequence qualified as S
import Std

withoutMutations ::
  FilePath ->
  Integer ->
  Integer ->
  Bool ->
  IO (Either Certainty (HashMap Identifier Value))
withoutMutations file nSubstitute searchDepth prune =
  fmap (boundedVerification searchDepth prune) $
  fmap (Gcl.instantiateN $ IntegerLiteral nSubstitute) $
  fmap Gcl.fromParseResult $
  fmap parse $
  fmap decodeUtf8 $
  readFileBS file

withMutations ::
  FilePath ->
  Integer ->
  Integer ->
  Bool ->
  IO ([Either Certainty (HashMap Identifier Value)])
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
