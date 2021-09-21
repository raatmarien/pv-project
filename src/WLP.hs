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
    AType x -> undefined

label :: Model -> Type -> AST -> Z3 (String, Maybe Concrete)
label m t f = astToString f >>= (evalType m t f <&>) . (,)

contradict :: ProgramPath -> IO ()
contradict p = evalZ3 $ do
    (wlp, env) <- wlpPath p empty
    let free = map snd $ toList env

    assert =<< mkNot wlp

    let model m = mapM (uncurry $ label m) free
    (res, sol) <- withModel model

    liftIO . putStrLn =<< astToString wlp
    liftIO $ print res -- "UnSat means no counterexample exists"

    case sol of
        Nothing -> liftIO $ putStrLn "No counterexample found"
        Just x  -> liftIO $ print x


wlpPath :: ProgramPath -> Env -> Z3 (AST, Env)
wlpPath [] e       = (,e) <$> mkTrue
wlpPath (x:xs) env = do
    let (wlpX, z3env') = wlpOne x env
    env' <- z3env'
    (rhs, env'') <- wlpPath xs env'
    (, env'') <$> wlpX rhs

wlpOne :: BasicStmt -> Env -> (AST -> Z3 AST, Z3 Env)
wlpOne (BVarDecl v t) e = (return, flip (insert v . (t, )) e <$> mkFreshFromType v t)
wlpOne (BAssert x) e    = ((symbolic e x >>=) . flip (mkBin mkAnd), return e)
wlpOne (BAssume x) e    = ((symbolic e x >>=) . flip mkImplies, return e)
wlpOne (BAssign v x) e  = (return, flip (insert v . (fst $ e ! v, )) e <$> symbolic e x)
wlpOne BAAssign{} _     = undefined
wlpOne BDrefAssign{} _  = undefined


mkFreshFromType :: String -> Type -> Z3 AST
mkFreshFromType v t = case t of
    PType x -> case x of
        PTInt -> mkFreshIntVar v
        PTBool -> mkFreshBoolVar v
    RefType -> undefined
    AType x -> undefined

symbolic :: Env -> Expr -> Z3 AST
symbolic e ex = case ex of
    Var v               -> return $ snd $ e ! v
    LitI i              -> mkInteger $ fromIntegral i
    LitB b              -> mkBool b
    LitNull             -> error "literally null"
    Parens x            -> undefined -- how dare you
    ArrayElem a i       -> undefined
    OpNeg x             -> mkNot =<< symbolic e x
    BinopExpr op x y    -> join $ mkBinop op <$> symbolic e x <*> symbolic e y
    Forall v x          -> makeQuantifier mkForallConst v e x
    Exists v x          -> makeQuantifier mkExistsConst v e x
    SizeOf a            -> undefined
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