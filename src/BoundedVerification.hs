module BoundedVerification where

import Gcl (Identifier)
import Gcl qualified
import Tree (Tree (Empty))
import Tree qualified
import Path qualified
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
  fmap fst . boundedVerificationWithPath searchDepth prune

boundedVerificationWithPath ::
  Integer ->
  Bool ->
  [Gcl.Statement] ->
  Maybe (HashMap Identifier Value, [Path.Statement])
boundedVerificationWithPath searchDepth prune =
  foldr (<|>) Nothing . -- short circuit
  fmap (\p -> (,p) <$> SymbolicExecution.counterExample p) .
  Tree.leaves .
  (if prune then (fromMaybe Empty .
                  Tree.pruneByFeasibility)
   else id) .
  Tree.pathsTree .
  fromMaybe Empty .
  Tree.pruneByLength searchDepth .
  Tree.statementTree .
  Gcl.addArrayAssignAssertions .
  Gcl.addIndexingAssertions .
  Gcl.rename

verificationLeavesAmount ::
  Integer ->
  Bool ->
  [Gcl.Statement] ->
  Int
verificationLeavesAmount searchDepth prune =
  length .
  Tree.leaves .
  (if prune then (fromMaybe Empty .
                  Tree.pruneByFeasibility)
   else id) .
  Tree.pathsTree .
  fromMaybe Empty .
  Tree.pruneByLength searchDepth .
  Tree.statementTree .
  Gcl.addArrayAssignAssertions .
  Gcl.addIndexingAssertions .
  Gcl.rename
