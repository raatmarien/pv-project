module RunFunctions where

import BoundedVerification (boundedVerification, mergeResults)
import GCLUtils (parseGCLstring, mutateProgram)
import Gcl (Type (PType), PrimitiveType (PTBool), Expression (IntegerLiteral), Identifier)
import Gcl qualified
import Path qualified
import Tree (Tree (Empty))
import Tree qualified
import SymbolicExecution (Value, Certainty (Certain), counterExample, symbolifyPath)

import Data.Sequence qualified as S
import Std

withoutMutations ::
  String ->
  Integer ->
  Integer ->
  Bool ->
  IO (Either String (Either Certainty (HashMap Identifier Value)))
withoutMutations file nSubstitute searchDepth prune =
  (fmap . fmap) (boundedVerification searchDepth prune) $
  (fmap . fmap) (Gcl.instantiateN $ IntegerLiteral nSubstitute) $
  fmap Gcl.fromParseResult $
  fmap (fromRight $ error "not testing the parser") $
  fmap parseGCLstring $
  fmap decodeUtf8 $
  readFileBS file

withMutations ::
  String ->
  Integer ->
  Integer ->
  Bool ->
  IO ([Either String (Either Certainty (HashMap Identifier Value))])
withMutations file nSubstitute searchDepth prune =
  (fmap . fmap . fmap) (boundedVerification searchDepth prune) $
  (fmap . fmap . fmap) (Gcl.instantiateN $ IntegerLiteral nSubstitute) $
  (fmap . fmap) Gcl.fromParseResult $
  (fmap . fmap) snd $
  fmap mutateProgram $
  fmap (fromRight $ error "not testing the parser") $
  fmap parseGCLstring $
  fmap decodeUtf8 $
  readFileBS file

verifyProgram :: String -> Integer -> Integer -> Bool -> IO ()
verifyProgram program n k prune = do
  result <- withoutMutations program n k prune
  print result

runMutateProgram :: String -> Integer -> Integer -> Bool -> IO ()
runMutateProgram program n k prune = do
  result <- withMutations program n k prune
  print result
