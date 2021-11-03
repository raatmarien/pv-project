module SymbolicExecution where

import Gcl
  ( BinOp
      ( Alias,
        And,
        Divide,
        Equal,
        GreaterThan,
        GreaterThanEqual,
        Implication,
        LessThan,
        LessThanEqual,
        Minus,
        Multiply,
        Or,
        Plus
      ),
    Expression (Variable),
    Identifier (Identifier),
    PrimitiveType (PTBool, PTInt),
    Type (AType, PType, RefType),
    foldExpression,
  )
import Path qualified

import Z3.Monad hiding (local)
import Data.HashMap.Strict qualified as HM
import System.IO.Unsafe (unsafePerformIO)
import Std hiding (Type)

data Statement =
  Assert Expression |
  Assume Expression
  deriving (Show)

newtype Environment =
  Environment (HashMap Identifier Z3Variable)

type Z3Variable =
  (Either
    (Z3Primitive {-current-}, Z3Primitive {-initial-}, PrimitiveType)
    (Z3Array {-current-}, Z3Array {-initial-}, PrimitiveType)
  )

newtype Z3Primitive = Z3Primitive AST

data Z3Array =
  Z3Array
    AST
    AST -- ^ length

type SymbolicExpression = ReaderT Environment Z3 AST

data Value =
  Integer Integer |
  Bool Bool |
  IntegerArray [Integer] |
  BoolArray [Bool]
  deriving (Show, Eq)

{-# noinline counterExample #-}
counterExample ::
  [Path.Statement] -> Maybe (HashMap Identifier Value)
counterExample statements =
  unsafePerformIO $ do
    x <- evalZ3 $ do 
        (z3Statements, Environment environment) <- symbolifyPath statements
        astToString =<< mkNot z3Statements

    writeFile "dump.txt" x

    evalZ3 $ do
      (z3Statements, Environment environment) <- symbolifyPath statements
      assert =<< mkNot z3Statements
      fmap parseZ3Result $ withModel $ \model ->
        traverse (evaluate model) environment
  where
    evaluate :: Model -> Z3Variable -> Z3 Value
    evaluate model (Left (_z3AstCurrent, Z3Primitive z3AstInitial, typ)) =
      case typ of
        PTInt ->
          fmap Integer $
          fmap
            (fromMaybe (error "all environment entries are known to Z3")) $
          evalInt model z3AstInitial
        PTBool ->
          fmap Bool $
          fmap
            (fromMaybe (error "all environment entries are known to Z3")) $
          evalBool model z3AstInitial
    evaluate model (Right (_current, Z3Array z3Array z3Length, typ)) =
      do
        indexes <-
          fmap (fromTo 0) $
          fmap
            (fromMaybe (error "all environment entries are known to Z3")) $
          evalInt model z3Length
        case typ of
          PTInt ->
            fmap IntegerArray $
              traverse
                (\index ->
                  fmap
                    (fromMaybe (error "all environment entries are known to Z3")) $
                  evalInt model =<< mkSelect z3Array =<< mkInteger index
                )
                indexes
          PTBool ->
            fmap BoolArray $
              traverse
                (\index ->
                  fmap
                    (fromMaybe (error "all environment entries are known to Z3")) $
                  evalBool model =<< mkSelect z3Array =<< mkInteger index
                )
                indexes
    parseZ3Result :: (Show a) => (Result, Maybe a) -> Maybe a
    parseZ3Result (Sat, Just a) = Just a
    parseZ3Result (Unsat, Nothing) = Nothing
    parseZ3Result (Undef, Nothing) = error "Is Z3's result ever undefined?"
    parseZ3Result result = error ("unsound Z3 result: " <> show result)

-- to-do. rewrite as foldr for better performance
symbolifyPath :: [Path.Statement] -> Z3 (AST, Environment)
symbolifyPath statementsAll =
  runStateT (go statementsAll) (Environment HM.empty)
  where
    go :: [Path.Statement] -> StateT Environment Z3 AST
    go [] = lift mkTrue
    go (statement : statementsRest) =
      case statement of
        -- special case powering array cloning
        Path.Assign identifierTarget (Variable identifierSource) ->
          do
            z3VariableSource <-
              liftReaderT (environmentLookup identifierSource) =<< get
            modify (environmentUpdate identifierTarget z3VariableSource)
            go statementsRest
        Path.Assign identifier expression ->
          do
            z3Ast <-
              liftReaderT (symbolifyExpression expression) =<< get
            modify
              (environmentUpdate identifier (Left $ Z3Primitive $ z3Ast))
            go statementsRest
        Path.AssignArray identifier index expression ->
          do
            Z3Array z3ArrayOld length <-
              fmap
                (fromRight
                  $ error "could not match expected array type with actual primtive type"
                ) $
              (liftReaderT (environmentLookup identifier) =<< get)
            z3ArrayNew <-
              bind2 (lift .: mkStore z3ArrayOld)
                (liftReaderT (symbolifyExpression index) =<< get)
                (liftReaderT (symbolifyExpression expression) =<< get)
            modify
              (environmentUpdate identifier (Right $ Z3Array z3ArrayNew length))
            go statementsRest
        Path.Declaration identifier@(Identifier identifierString _ _) typ ->
          do
            z3Variable <-
              lift $
              case typ of
                PType primitiveType ->
                  fmap Left $
                  fmap (, primitiveType) $
                  fmap Z3Primitive $
                  mkFreshConst identifierString =<< z3Sort primitiveType
                AType primitiveType ->
                  fmap Right $
                  fmap (, primitiveType) $
                  Z3Array
                    <$>
                      (mkFreshConst
                        identifierString
                        =<< bind2 mkArraySort mkIntSort (z3Sort primitiveType)
                      )
                    <*> mkFreshIntVar ("z" <> identifierString)
                RefType -> error "references not implemented"
            modify (environmentInsert identifier z3Variable)
            go statementsRest
          where
            z3Sort :: PrimitiveType -> Z3 Sort
            z3Sort PTInt = mkIntSort
            z3Sort PTBool = mkBoolSort
        Path.Assert expression ->
          do
            environment <- get
            symbolicRest <-  go statementsRest
            liftReaderT
              (weakestPrecondition (Assert expression) symbolicRest)
              environment
        Path.Assume expression ->
          do
            environment <- get
            symbolicRest <- go statementsRest
            liftReaderT
              (weakestPrecondition (Assume expression) symbolicRest)
              environment

weakestPrecondition :: Statement -> AST -> SymbolicExpression
weakestPrecondition statement postcondition =
  case statement of
    Assert expression ->
      mkAnd =<< (symbolifyExpression expression <&> (: [postcondition]))
    Assume expression ->
      symbolifyExpression expression >>= (`mkImplies` postcondition)

symbolifyVariable :: Identifier -> SymbolicExpression
symbolifyVariable identifier =
  environmentLookup identifier
    <&>
      \case
        Left (Z3Primitive primitiveZ3) -> primitiveZ3
        Right (Z3Array {}) ->
          error
            "could not match expected primtive type with actual array type"

symbolifyArrayLength :: Identifier -> SymbolicExpression
symbolifyArrayLength identifier =
  environmentLookup identifier
    <&>
      \case
        Left (Z3Primitive {}) ->
          error "could not match expected array type with actual primtive type"
        Right (Z3Array _ length) -> length

symbolifyBinaryOperation ::
  BinOp -> SymbolicExpression -> SymbolicExpression -> SymbolicExpression
symbolifyBinaryOperation operator expression0 expression1 =
  do
    expression0Z3 <- expression0
    expression1Z3 <- expression1
    expression0Z3 `operatorZ3` expression1Z3
  where
    operatorZ3 :: AST -> AST -> SymbolicExpression
    operatorZ3 =
      case operator of
        Plus -> \a b -> mkAdd [a, b]
        Minus -> \a b -> mkSub [a, b]
        Multiply -> \a b -> mkMul [a, b]
        Divide -> mkDiv
        LessThan -> mkLt
        LessThanEqual -> mkLe
        GreaterThan -> mkGt
        GreaterThanEqual -> mkGe
        And -> \a b -> mkAnd [a, b]
        Or -> \a b -> mkOr [a, b]
        Implication -> mkImplies
        Equal -> mkEq
        Alias -> error "no aliases"

symbolifyIndexing :: Identifier -> SymbolicExpression -> SymbolicExpression
symbolifyIndexing identifier expression =
  do
    variableZ3 <- environmentLookup identifier
    case variableZ3 of
      Left (Z3Primitive {}) ->
        error
          "could not match expected array type with actual primitive type"
      Right (Z3Array z3Array _length) -> mkSelect z3Array =<< expression

symbolifyForall :: Identifier -> SymbolicExpression -> SymbolicExpression
symbolifyForall identifier@(Identifier identifierString _ _) expression =
  do
    variable <- mkFreshIntVar identifierString
    variable' <- toApp variable
    local
      (environmentInsert identifier $ Left (Z3Primitive variable, PTInt))
      (mkForallConst [] [variable'] =<< expression)

symbolifyExists :: Identifier -> SymbolicExpression -> SymbolicExpression
symbolifyExists identifier@(Identifier identifierString _ _) expression =
  do
    variable <- mkFreshIntVar identifierString
    variable' <- toApp variable
    local
      (environmentInsert identifier $ Left (Z3Primitive variable, PTInt))
      (mkExistsConst [] [variable'] =<< expression)

symbolifyExpression :: Expression -> SymbolicExpression
symbolifyExpression =
  foldExpression
    mkInteger
    mkBool
    symbolifyVariable
    symbolifyArrayLength
    symbolifyBinaryOperation
    (mkNot =<<)
    symbolifyIndexing
    symbolifyForall
    symbolifyExists

environmentInsert ::
  Identifier ->
  Either (Z3Primitive, PrimitiveType) (Z3Array, PrimitiveType) ->
  Environment ->
  Environment
environmentInsert
  identifier@(Identifier identifierString _ _)
  z3Ast
  (Environment environment)
  | identifier `HM.member` environment =
    error
      ("unexpected environment overwrite at " <> toText identifierString)
  | otherwise =
    Environment $
    HM.insert identifier (bimap backupZ3Ast backupZ3Ast z3Ast) environment
  where
    backupZ3Ast (z3Ast, typ) = (z3Ast, z3Ast, typ)

environmentLookup ::
  (Monad m) =>
  Identifier -> ReaderT Environment m (Either Z3Primitive Z3Array)
environmentLookup identifier@(Identifier identifierString _ _) =
  reader $
    \(Environment environment) ->
      case HM.lookup identifier environment of
        Nothing -> error ("unknown identifier " <> toText identifierString)
        Just z3Ast -> bimap current current z3Ast
  where
    current (z3AstCurrent, _initial, _type) = z3AstCurrent

environmentUpdate ::
  Identifier -> Either Z3Primitive Z3Array -> Environment -> Environment
environmentUpdate
  identifier@(Identifier identifierString _ _)
  z3AstNew
  (Environment environment)
  = Environment (HM.alter update identifier environment)
  where
    update Nothing =
      error
        ("assignment to unknown identifier " <> toText identifierString)
    update (Just z3AstOld) =
      case (z3AstOld, z3AstNew) of
        (Left (_old, primitiveZ3Initial, typ), Left primitiveZ3New) ->
          Just $ Left $ (primitiveZ3New, primitiveZ3Initial, typ)
        (Right (_old, arrayZ3Initial, typ), Right arrayZ3New) ->
          Just $ Right $ (arrayZ3New, arrayZ3Initial, typ)
        (Left _primitiveZ3, Right _arrayZ3) ->
          error "do not assign an array type to a primitive type"
        (Right _arrayZ3, Left _primitiveZ3) ->
          error "do not assign a primitive type to an array type"

liftReaderT ::
  (Monad monad) =>
  ReaderT environment monad a -> environment -> StateT state monad a
liftReaderT = lift .: runReaderT
