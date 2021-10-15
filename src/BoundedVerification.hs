module BoundedVerification where

import Gcl (Identifier)
import Gcl qualified
import Tree (Tree (Empty))
import Tree qualified
import SymbolicExecution (Value)
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
  Maybe (HashMap Identifier Value)
boundedVerification searchDepth prune =
  foldr (<|>) Nothing . -- short circuit
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
