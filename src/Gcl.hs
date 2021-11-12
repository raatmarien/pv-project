{-# OPTIONS_GHC
  -Wno-orphans
#-}

module Gcl (module Gcl, Type (..), PrimitiveType (..), BinOp (..)) where

import GCLParser.GCLDatatype qualified as Parse
import GCLParser.GCLDatatype
  ( BinOp (..),
    PrimitiveType (..),
    Type (..),
  )
import Control.Lens
import Data.Data.Lens (template)
import qualified Data.List.NonEmpty as N
import Data.Sequence qualified as S
import Data.Data (Data)
import Std hiding (Type)

data Statement =
  Assert Expression |
  Assume Expression |
  Assign Identifier Expression |
  AssignArray Identifier Expression Expression |
  AppendArray Identifier Expression |
  If Expression [Statement] [Statement] |
  While Expression [Statement] |
  Declarations (NonEmpty (Identifier, Type)) [Statement]
  deriving (Show, Data)

data Identifier =
  Identifier
    String
    Integer -- ^ id implementing lexical scoping
    Integer -- ^ id distinguishing lexically identical variables in
    -- distinct loop iterations
  deriving (Show, Eq, Data, Generic)
instance Hashable Identifier

data Expression =
  IntegerLiteral Integer |
  BoolLiteral Bool |
  Variable Identifier |
  ArrayLength Identifier |
  BinaryOperation BinOp Expression Expression |
  Negation Expression |
  Indexing Identifier Expression |
  Forall Identifier Expression |
  Exists Identifier Expression
  deriving (Show, Data)

deriving instance Data Type
deriving instance Data BinOp
deriving instance Data PrimitiveType


fromParseResult :: Parse.Program -> [Statement]
fromParseResult (Parse.Program _name input output programParsed) =
  either (error . toText) id $
  fmap makeHeap $
  fmap parametersToDeclarations $
  fromParsedProgram programParsed
  where
    parametersToDeclarations :: [Statement] -> [Statement]
    parametersToDeclarations =
      case fromParsedDeclarations (input <> output) of
        Nothing -> id
        Just d -> pure . Declarations d . prependHeapAssumptions d

    prependHeapAssumptions :: NonEmpty (Identifier, Type) -> [Statement] -> [Statement]
    prependHeapAssumptions d xs = map (heapHyp . fst) (N.filter ((== RefType) . snd) d) ++ xs

    heapHyp identifier = Assume (BinaryOperation And(BinaryOperation And
      (BinaryOperation LessThan (ArrayLength heapId) (IntegerLiteral 5))
      (BinaryOperation LessThan (Variable identifier) (ArrayLength heapId)))
      (BinaryOperation LessThanEqual (IntegerLiteral (-1)) (Variable identifier)))


heapId :: Identifier
heapId = Identifier "heap" (-2) 0

stringToId :: String -> Identifier
stringToId identifier = Identifier identifier (-1) 0


fromParsedProgram :: Parse.Stmt -> Either String [Statement]
fromParsedProgram program =
  case program of
    Parse.Skip -> Right []
    Parse.Assert expression ->
      fmap pure $ Assert <$> fromParsedExpression expression
    Parse.Assume expression ->
      fmap pure $ Assume <$> fromParsedExpression expression
    Parse.Assign identifier (Parse.NewStore expression) ->
      sequence [
        return $ Assign (Identifier identifier (-1) 0) (ArrayLength heapId),
        AppendArray heapId
          <$> fromParsedExpression expression
      ]
    Parse.Assign identifier expression ->
      fmap pure $
        Assign (Identifier identifier (-1) 0)
          <$> fromParsedExpression expression
    Parse.AAssign identifier index expression ->
      fmap pure $
        AssignArray (Identifier identifier (-1) 0)
          <$> fromParsedExpression index
          <*> fromParsedExpression expression
    Parse.DrefAssign identifier expression ->
      fmap pure $
        AssignArray heapId
          <$> fromParsedExpression (Parse.Var identifier)
          <*> fromParsedExpression expression
    Parse.Seq program0 program1 ->
      (<>) <$> fromParsedProgram program0 <*> fromParsedProgram program1
    Parse.IfThenElse guard branch0 branch1 ->
      fmap pure $
        If
          <$> fromParsedExpression guard
          <*> fromParsedProgram branch0
          <*> fromParsedProgram branch1
    Parse.While guard body ->
      fmap pure $
        While <$> fromParsedExpression guard <*> fromParsedProgram body
    Parse.Block declarationsParsed body ->
      fmap pure $ Declarations <$> declarations <*> fromParsedProgram body
      where
        declarations =
          case fromParsedDeclarations declarationsParsed of
            Nothing -> Left "no empty declarations"
            Just d -> Right d
    Parse.TryCatch {} -> Left "exceptions not implemented"

fromParsedExpression :: Parse.Expr -> Either String Expression
fromParsedExpression =
  \case
    Parse.Var identifier -> Right $ Variable (Identifier identifier (-1) 0)
    Parse.LitI integer ->
      Right $ IntegerLiteral $ fromIntegral $ integer
    Parse.LitB bool -> Right $ BoolLiteral $ bool
    Parse.LitNull -> Right $ IntegerLiteral (-1)
    Parse.Parens expression -> fromParsedExpression expression
    Parse.ArrayElem (Parse.Var identifier) index ->
      Indexing
        (Identifier identifier (-1) 0)
        <$> fromParsedExpression index
    Parse.ArrayElem {} ->
      error "could not match expected array type with actual primtive type"
    Parse.OpNeg expression -> Negation <$> fromParsedExpression expression
    Parse.BinopExpr binaryOperator expression0 expression1 ->
      BinaryOperation
        binaryOperator
        <$> fromParsedExpression expression0
        <*> fromParsedExpression expression1
    Parse.Forall identifier expression ->
      Forall (Identifier identifier (-1) 0) <$> fromParsedExpression expression
    Parse.Exists identifier expression ->
      Exists (Identifier identifier (-1) 0) <$> fromParsedExpression expression
    Parse.SizeOf (Parse.Var identifier) ->
      Right $ ArrayLength (Identifier identifier (-1) 0)
    Parse.SizeOf _ -> Left "apply length operators to variables only"
    Parse.RepBy {} -> Left "no `RepBy`s"
    Parse.Cond {} -> Left "no `Cond`s"
    Parse.NewStore {} -> Left "NewStore should be intercepted in fromParsedProgram"
    Parse.Dereference identifier ->
      Indexing
        heapId
        <$> fromParsedExpression (Parse.Var identifier)


fromParsedDeclarations ::
  [Parse.VarDeclaration] -> Maybe (NonEmpty (Identifier, Type))
fromParsedDeclarations =
  nonEmpty
  .
  fmap
    (\(Parse.VarDeclaration identifier typ) ->
      (Identifier identifier (-1) 0, typ)
    )

rename :: [Statement] -> [Statement]
rename = evaluatingState 0 . renameHelp

renameHelp :: [Statement] -> State Integer [Statement]
-- `reverse` to order identifier ids as the identifiers occur in the
-- source code
renameHelp = fmap reverse . traverse renameStatement . reverse

renameStatement :: Statement -> State Integer Statement
renameStatement =
  \case
    Declarations declarations statementsOld ->
      do
        statementsRenamed <- renameHelp statementsOld
        modify (+1)
        get <&> \id ->
          let
            renameIdentifiers :: (Data s) => s -> s
            renameIdentifiers =
              compose $
              toList $
              fmap (renameIdentifier id) $
              fmap fst $
              declarations
          in
            Declarations
              (renameIdentifiers declarations)
              (renameIdentifiers statementsRenamed)
    If guard statementsOld0 statementsOld1 ->
      -- `flip` for the same reason as `reverse` in renameHelp
      flip (If guard)
        <$> renameHelp statementsOld1
        <*> renameHelp statementsOld0
    While guard statementsOld ->
      While guard <$> renameHelp statementsOld
    statement -> pure statement

renameIdentifier :: (Data s) => Integer -> Identifier -> s -> s
renameIdentifier id identifier =
  let
    go identifierCurrent@(Identifier identifierTextCurrent idOld iterationId)
      | identifierCurrent == identifier,
        idOld != (-2) -- generated identifiers inaccessible to the user
        = Identifier identifierTextCurrent id iterationId
      | otherwise = identifierCurrent
  in over template go

renameUnfolding :: [Statement] -> [Statement]
renameUnfolding =
  fmap
    (\case
      Declarations declarations statements ->
        Declarations
          (renameIdentifiers declarations)
          (renameIdentifiers $ renameUnfolding $ statements)
        where
          renameIdentifiers :: (Data s) => s -> s
          renameIdentifiers =
            compose $
            toList $
            fmap renameIdentifierUnfolding $
            fmap fst $
            declarations
      If guard statements0 statements1 ->
        If
          guard
          (renameUnfolding statements0)
          (renameUnfolding statements1)
      While guard statements ->
        While guard (renameUnfolding statements)
      statement -> statement
    )

renameIdentifierUnfolding :: (Data s) => Identifier -> s -> s
renameIdentifierUnfolding identifier =
  let
    go identifierCurrent@(Identifier identifierTextCurrent id iterationId)
      | identifierCurrent == identifier =
        Identifier identifierTextCurrent id (iterationId+1)
      | otherwise = identifierCurrent
  in over template go

addIndexingAssertions :: [Statement] -> [Statement]
addIndexingAssertions [] = []
addIndexingAssertions (statement : statementsRest) =
  case statement of
    assert@(Assert {}) -> assert : addIndexingAssertions statementsRest
    assume@(Assume {}) -> assume : addIndexingAssertions statementsRest
    assign@(Assign _identifier expression) ->
      toList (addIndexingAssertionsExpression expression) <>
      [assign] <>
      addIndexingAssertions statementsRest
    assignArray@(AssignArray _identifier _index expression) ->
      toList (addIndexingAssertionsExpression expression) <>
      [assignArray] <>
      addIndexingAssertions statementsRest
    If guard branch0 branch1 ->
      toList (addIndexingAssertionsExpression guard) <>
      [If
        guard
        (addIndexingAssertions branch0)
        (addIndexingAssertions branch1)
      ] <>
      addIndexingAssertions statementsRest
    While guard statements ->
      toList (addIndexingAssertionsExpression guard) <>
      [While
        guard
        (addIndexingAssertions statements)
      ] <>
      addIndexingAssertions statementsRest
    Declarations declarations statements ->
      Declarations declarations (addIndexingAssertions statements)
      :
      addIndexingAssertions statementsRest
    appendArray@(AppendArray {}) -> appendArray : addIndexingAssertions statementsRest

addIndexingAssertionsExpression :: Expression -> Seq Statement
addIndexingAssertionsExpression =
  \case
    IntegerLiteral {} -> S.empty
    BoolLiteral {} -> S.empty
    Variable {} -> S.empty
    ArrayLength {} -> S.empty
    BinaryOperation _binaryOperator expression0 expression1 ->
      addIndexingAssertionsExpression expression0 <>
      addIndexingAssertionsExpression expression1
    Negation expression -> addIndexingAssertionsExpression expression
    Indexing identifier expression ->
      indexAssertion identifier expression <|
      addIndexingAssertionsExpression expression
    Forall {} ->
      error "do not use quantifiers outside assumptions and assertions"
    Exists {} ->
      error "do not use quantifiers outside assumptions and assertions"

indexAssertion :: Identifier -> Expression -> Statement
indexAssertion identifier index =
  Assert (BinaryOperation And
  (BinaryOperation LessThan index (ArrayLength identifier))
  (BinaryOperation LessThanEqual (IntegerLiteral 0) index))

addArrayAssignAssertions :: [Statement] -> [Statement]
addArrayAssignAssertions [] = []
addArrayAssignAssertions (statement : statementsRest) =
  case statement of
    assert@(Assert {}) -> assert : addArrayAssignAssertions statementsRest
    assume@(Assume {}) -> assume : addArrayAssignAssertions statementsRest
    assign@(Assign {}) -> assign : addArrayAssignAssertions statementsRest
    appendArray@(AppendArray {}) -> appendArray : addArrayAssignAssertions statementsRest
    assignArray@(AssignArray identifier index _expression) ->
      indexAssertion identifier index :
      assignArray :
      addArrayAssignAssertions statementsRest
    If guard branch0 branch1 ->
      If
        guard
        (addArrayAssignAssertions branch0)
        (addArrayAssignAssertions branch1)
      :
      addArrayAssignAssertions statementsRest
    While guard statements ->
      While guard (addArrayAssignAssertions statements)
      :
      addArrayAssignAssertions statementsRest
    Declarations declarations statements ->
      Declarations declarations (addArrayAssignAssertions statements)
      :
      addArrayAssignAssertions statementsRest

foldExpression ::
  (Integer -> result) ->
  (Bool -> result) ->
  (Identifier -> result) ->
  (Identifier -> result) ->
  (BinOp -> result -> result -> result) ->
  (result -> result) ->
  (Identifier -> result -> result) ->
  (Identifier -> result -> result) ->
  (Identifier -> result -> result) ->
  Expression ->
  result
foldExpression
  integerLiteral
  boolLiteral
  variable
  arrayLength
  binaryOperation
  negation
  indexing
  forall
  exists
  = \case
    IntegerLiteral integer -> integerLiteral integer
    BoolLiteral bool -> boolLiteral bool
    Variable identifier -> variable identifier
    ArrayLength identifier -> arrayLength identifier
    BinaryOperation binaryOperator expression0 expression1 ->
      binaryOperation
        binaryOperator
        (recurse expression0)
        (recurse expression1)
    Negation expression -> negation (recurse expression)
    Indexing identifier expression ->
      indexing identifier (recurse expression)
    Forall identifier expression -> forall identifier (recurse expression)
    Exists identifier expression -> exists identifier (recurse expression)
  where
    recurse =
      foldExpression
        integerLiteral
        boolLiteral
        variable
        arrayLength
        binaryOperation
        negation
        indexing
        forall
        exists

instantiateN :: Expression -> [Statement] -> [Statement]
instantiateN substitute = over template (instantiateNExpression substitute)

instantiateNExpression :: Expression -> Expression -> Expression
instantiateNExpression substitute =
  foldExpression
    IntegerLiteral
    BoolLiteral
    replaceN
    ArrayLength
    BinaryOperation
    Negation
    Indexing
    Forall
    Exists
  where
    replaceN (Identifier "N" (-1) 0) = substitute
    replaceN identifier = Variable identifier


makeHeap :: [Statement] -> [Statement]
makeHeap = pure . Declarations ((heapId, AType PTInt) :| [])
