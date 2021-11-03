module Tree where

import Gcl (Expression (Negation), Expression (BoolLiteral), renameUnfolding)
import Gcl qualified
import Path qualified
import SymbolicExecution (counterExample)

import Data.Sequence qualified as S
import Std hiding (Type)

data Tree a =
  Empty |
  Path a (Tree a) |
  Bifurcation a (Tree a) a (Tree a)
  deriving (Show, Functor)

statementTree :: [Gcl.Statement] -> Tree Path.Statement
statementTree [] = Empty
statementTree (statement : statementsRest) =
  case statement of
    Gcl.Assert expression ->
      Path (Path.Assert expression) (statementTree statementsRest)
    Gcl.Assume expression ->
      Path (Path.Assume expression) (statementTree statementsRest)
    Gcl.Assign identifier expression ->
      Path
        (Path.Assign identifier expression)
        (statementTree statementsRest)
    Gcl.AssignArray identifier index expression ->
      Path
        (Path.AssignArray identifier index expression)
        (statementTree statementsRest)
    Gcl.AppendArray identifier expression ->
      Path (Path.AppendArray identifier expression) (statementTree statementsRest)
    Gcl.Declarations declarations statements ->
      foldr1 (.) (uncurry (Path .: Path.Declaration) <$> declarations)
        $ statementTree (statements <> statementsRest)
    Gcl.If guard branch0 branch1 ->
      Bifurcation
        (Path.Assume guardShorter)
        (statementTree $ branchShorter <> statementsRest)
        (Path.Assume guardLonger)
        (statementTree $ branchLonger <> statementsRest)
      where
        (guardShorter, guardLonger) =
          if length branch0 <= length branch1
          then (guard, Negation guard)
          else (Negation guard, guard)
        (branchShorter, branchLonger) =
          if length branch0 <= length branch1
          then (branch0, branch1)
          else (branch1, branch0)
    Gcl.While guard body ->
      statementTree
        (
          Gcl.If
            guard
            (body <> [Gcl.While guard (renameUnfolding body)])
            []
          :
          statementsRest
        )

pruneByLength :: Integer -> Tree a -> Maybe (Tree a)
pruneByLength k = prune (prunePathByLength k)

prunePathByLength ::
  Integer ->
  a ->
  Tree a ->
  Maybe (a, Tree a)
prunePathByLength k _ _ | k <= 0 = Nothing
prunePathByLength k a rest =
  (a,) <$> pruneByLength (k-1) rest

prune ::
  (a -> Tree a -> Maybe (a, Tree a)) -> Tree a -> Maybe (Tree a)
prune prunePath =
  \case
    Empty -> Just Empty
    Path statements rest ->
      uncurry Path <$> prunePath statements rest
    Bifurcation statements0Old rest0Old statements1Old rest1Old ->
      case
        (
          prunePath statements0Old rest0Old
          ,
          prunePath statements1Old rest1Old
        )
      of
        (Just (statements0, rest0), Just (statements1, rest1)) ->
          Just (Bifurcation statements0 rest0 statements1 rest1)
        (Just (statements0, rest0), Nothing) ->
          Just (Path statements0 rest0)
        (Nothing, Just (statements1, rest1)) ->
          Just (Path statements1 rest1)
        (Nothing, Nothing) -> Nothing

pathsTree :: Tree Path.Statement -> Tree [Path.Statement]
pathsTree =
  \case
    Empty -> Empty
    Path statement rest ->
      Path
        [statement]
        (fmap (statement :) $ pathsTree rest)
    Bifurcation statement0 rest0 statement1 rest1 ->
      Bifurcation
        [statement0]
        (fmap (statement0 :) $ pathsTree rest0)
        [statement1]
        (fmap (statement1 :) $ pathsTree rest1)

pruneByFeasibility :: Tree [Path.Statement] -> Maybe (Tree [Path.Statement])
pruneByFeasibility = prune prunePathByFeasibility

prunePathByFeasibility ::
  [Path.Statement] ->
  Tree [Path.Statement] ->
  Maybe ([Path.Statement], Tree [Path.Statement])
prunePathByFeasibility statements rest@(Bifurcation _ _ _ _) =
  case
    counterExample
      (
        filter isNotAssert statements
        <>
        [Path.Assert $ BoolLiteral $ False]
      )
  of
    Nothing -> Nothing
    _ -> (statements,) <$> pruneByFeasibility rest
  where
    isNotAssert (Path.Assert _) = False
    isNotAssert _ = True
prunePathByFeasibility statements rest = (statements,) <$> pruneByFeasibility rest

leaves :: Tree a -> Seq a
leaves Empty = S.Empty
leaves (Path statements Empty) = S.singleton statements
leaves (Path _statements rest) = leaves rest
leaves (Bifurcation statements0 rest0 statements1 rest1) =
  leaves (Path statements0 rest0) <> leaves (Path statements1 rest1)
