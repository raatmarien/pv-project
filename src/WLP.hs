{-# OPTIONS_GHC -Wno-unused-matches #-}
module WLP

where


import Control.Monad
import Control.Monad.IO.Class

import Data.Map ( (!), empty, insert, toList, Map )
import Data.Functor

import Z3.Monad


import ProgramPath

import GCLParser.GCLDatatype


type Env = Map String (Type, AST)
data Concrete = IntC Integer | BoolC Bool deriving Show


evalType :: Model -> Type -> AST -> Z3 (Maybe Concrete)
evalType m t f = case t of
    PType x -> case x of
        PTInt -> (IntC <$>) <$> evalInt m f
        PTBool -> (BoolC <$>) <$> evalBool m f
    RefType -> undefined
    AType x -> return Nothing -- TODO represent this

label :: Model -> Type -> AST -> Z3 (String, Maybe Concrete)
label m t f = astToString f >>= (evalType m t f <&>) . (,)

contradict :: ProgramPath -> IO Bool
contradict p = evalZ3 $ do
    (wlp, env) <- wlpPath p empty
    let free = map snd $ toList env

    assert =<< mkNot wlp

    let model m = mapM (uncurry $ label m) free
    (res, sol) <- withModel model

    case sol of
        Nothing -> do
          -- putStrLn "The program path is valid."
          return False
        Just x  -> do
          -- putStrLn "The program path is invalid.\nCounter example:"
          -- print x
          return True

wlpPath :: ProgramPath -> Env -> Z3 (AST, Env)
wlpPath [] e       = (,e) <$> mkTrue
wlpPath (x:xs) env = do
    let (wlpX, z3env') = wlpOne x env
    env' <- z3env'
    (rhs, env'') <- wlpPath xs env'
    (, env'') <$> wlpX rhs

wlpOne :: BasicStmt -> Env -> (AST -> Z3 AST, Z3 Env)
wlpOne (BVarDecl v t) e = (return, mkFreshFromType v t e)
wlpOne (BAssert x) e    = ((symbolic e x >>=) . flip (mkBin mkAnd), return e)
wlpOne (BAssume x) e    = ((symbolic e x >>=) . flip mkImplies, return e)
wlpOne (BAssign v x) e  = (return, flip (insert v . (fst $ e ! v, )) e <$> symbolic e x)
wlpOne (BAAssign v i x) e = (return,
                             do index <- symbolic e i
                                value <- symbolic e x
                                let (t, oldArray) = e ! v
                                newArray <- mkStore oldArray index value
                                return $ insert v (t, newArray) e)
wlpOne BDrefAssign{} _  = undefined


mkFreshFromType :: String -> Type -> Env -> Z3 Env
mkFreshFromType v t env = case t of
    PType x -> case x of
        PTInt -> do
          var <- mkFreshIntVar v
          return $ insert v (t, var) env
        PTBool -> do
          var <- mkFreshBoolVar v
          return $ insert v (t, var) env
    RefType -> undefined
    AType x -> do
      intSort <- mkIntSort
      boolSort <- mkBoolSort
      arraySort <- mkArraySort intSort
                   (if x == PTInt then intSort else boolSort)
      a <- mkFreshVar v arraySort
      len_a <- mkFreshVar ("#"++v) intSort
      return $ insert v (t, a) $ insert ("#"++v) (PType PTInt, len_a) env

symbolic :: Env -> Expr -> Z3 AST
symbolic e ex = case ex of
    Var v               -> return $ snd $ e ! v
    LitI i              -> mkInteger $ fromIntegral i
    LitB b              -> mkBool b
    LitNull             -> error "literally null"
    Parens x            -> symbolic e x -- how dare you
    ArrayElem a i       -> do
      array <- symbolic e a
      index <- symbolic e i
      mkSelect array index
    OpNeg x             -> mkNot =<< symbolic e x
    BinopExpr op x y    -> join $ mkBinop op <$> symbolic e x <*> symbolic e y
    Forall v x          -> makeQuantifier mkForallConst v e x
    Exists v x          -> makeQuantifier mkExistsConst v e x
    SizeOf (Var v)      -> return $ snd $ e ! ("#" ++ v)
    SizeOf _            -> error "How do we represent the length of an unknown array?" -- TODO
    RepBy a i x         -> undefined
    Cond c a b          -> undefined
    NewStore x          -> undefined
    Dereference v       -> undefined


mkBin :: ([AST] -> Z3 AST) -> AST -> AST -> Z3 AST
mkBin op a b = op [a, b]


mkBinop :: BinOp -> AST -> AST -> Z3 AST
mkBinop op = case op of
    And                 -> mkBin mkAnd
    Or                  -> mkBin mkOr
    Implication         -> mkImplies
    LessThan            -> mkLt
    LessThanEqual       -> mkLe
    GreaterThan         -> mkGt
    GreaterThanEqual    -> mkGe
    Equal               -> mkEq
    Minus               -> mkBin mkSub
    Plus                -> mkBin mkAdd
    Multiply            -> mkBin mkMul
    Divide              -> mkDiv
    Alias               -> undefined

type MkQuantifier = [Pattern] -> [App] -> AST -> Z3 AST


makeQuantifier :: MkQuantifier -> String -> Env -> Expr -> Z3 AST
makeQuantifier mk v e x = do
    i <- mkFreshIntVar v
    let e' = insert v (PType PTInt, i) e -- note that the altered environment does not escape this quantified expression!

    i' <- toApp i
    x' <- symbolic e' x
    mk [] [i'] x'

{-
quantifyAll :: [App] -> MkQuantifier -> AST -> Z3 AST
quantifyAll as mk = mk [] as-}
