module BoundedVerification where

import Gcl (Identifier)
import Gcl qualified
import Tree (Tree (Empty))
import Tree qualified
import SymbolicExecution (Certainty (Uncertain, Certain), Value)
import SymbolicExecution qualified

import GCLUtils (parseGCLstring)
import GCLParser.GCLDatatype qualified as Parse
import Std

parse :: String -> Parse.Program
parse = either (error . toText) id . parseGCLstring

boundedVerification ::
  Integer ->
  Bool ->
  [Gcl.Statement] ->
  Either Certainty (HashMap Identifier Value)
boundedVerification searchDepth prune =
  (foldr mergeResults (Left Certain)) . -- short circuit
  fmap SymbolicExecution.counterExample .
  Tree.leaves .
  (if prune then ((fromMaybe Empty) .
                  Tree.pruneByFeasibility)
   else id) .
  Tree.pathsTree .
  (fromMaybe Empty) .
  (Tree.pruneByLength searchDepth) .
  Tree.statementTree .
  Gcl.addArrayAssignAssertions .
  Gcl.addIndexingAssertions .
  Gcl.rename

mergeResults :: Either Certainty a -> Either Certainty a -> Either Certainty a
mergeResults counterExample@(Right _) _ = counterExample -- short circuit
mergeResults _ counterExample@(Right _) = counterExample
mergeResults uncertain@(Left Uncertain) _ = uncertain
mergeResults _result0 result1 = result1
