module BoundedVerification where

import Gcl (Identifier)
import Gcl qualified
import Tree (Tree (Empty))
import Tree qualified
import SymbolicExecution (Certainty (Uncertain, Certain), Value)
import SymbolicExecution qualified

import Std

boundedVerification ::
  Integer ->
  [Gcl.Statement] ->
  Either Certainty (HashMap Identifier Value)
boundedVerification searchDepth =
  (foldr mergeResults (Left Certain)) . -- short circuit
  fmap SymbolicExecution.counterExample .
  Tree.leaves .
  (fromMaybe Empty) .
  Tree.pruneByFeasibility .
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
