module Z3 (
    BoolExpr(..)
  , CompType(..)
  , ArithExpr(..)
  , CheckResult(..)
  , mkAssertion
  , checkAndGetModel
  , getDoubles
  ) where

import Control.Monad.State
import Data.List (foldl', intercalate)
import Data.Map (Map, (!))
import qualified Data.Map as M

import Z3.LowLevel

data ArithExpr d =
    Add [ArithExpr d]
  | Mul [ArithExpr d]
  | Sub (ArithExpr d) (ArithExpr d)
  | Div (ArithExpr d) (ArithExpr d)
  | ToReal (ArithExpr d)
  | RealConst Int Int
  | RealVar d
  | IntConst Int
  | IntVar d
  deriving (Eq)

instance Show d => Show (ArithExpr d) where
  show expr = "(" ++ show' expr ++ ")" where
    show' (Add es) = intercalate " + " $ map show es
    show' (Mul es) = intercalate " * " $ map show es
    show' (Sub a b) = show a ++ " - " ++ show b
    show' (Div a b) = show a ++ " / " ++ show b
    show' (ToReal a) = "to_real " ++ show a
    show' (RealConst n d) = show n ++ "/" ++ show d
    show' (RealVar d) = "real-" ++ show d
    show' (IntConst n) = show n
    show' (IntVar d) = "int-" ++ show d

data CompType = Lt | Le | Eq | Ge | Gt | Neq
  deriving (Eq)

instance Show CompType where
  show Lt = "<"
  show Le = "<="
  show Eq = "=="
  show Ge = ">="
  show Gt = ">"
  show Neq = "/="

data BoolExpr d =
    And [BoolExpr d]
  | Or [BoolExpr d]
  | Not (BoolExpr d)
  | Implies (BoolExpr d) (BoolExpr d)
  | Comparison CompType (ArithExpr d) (ArithExpr d)
  | BoolConst Bool
  | BoolVar d
  deriving (Eq)

instance Show d => Show (BoolExpr d) where
  show expr = "(" ++ show' expr ++ ")" where
    show' (And es) = intercalate " && " $ map show es
    show' (Or es) = intercalate " || " $ map show es
    show' (Not e) = "!" ++ show e
    show' (Implies a b) = show a ++ " -> " ++ show b
    show' (Comparison op a b) = show a ++ " " ++ show op ++ " " ++ show b
    show' (BoolConst b) = show b
    show' (BoolVar d) = "bool-" ++ show d

data CheckResult d =
    Unsat
  | Sat (Map d AST)
  | Undef
  deriving (Eq, Show)

arithVariableMap :: (Ord d, Show d) => Context -> ArithExpr d
                 -> StateT (Map d AST) IO ()
arithVariableMap ctx expr =
  case expr of
    Add es -> mapM_ (arithVariableMap ctx) es
    Mul es -> mapM_ (arithVariableMap ctx) es
    Sub a b -> arithVariableMap ctx a >> arithVariableMap ctx b
    Div a b -> arithVariableMap ctx a >> arithVariableMap ctx b
    ToReal a -> arithVariableMap ctx a
    RealConst _ _ -> return ()
    RealVar v -> do
      mp <- get
      case M.lookup v mp of
        Nothing -> do
          var <- liftIO $ mkRealVar ctx (show v)
          put $ M.insert v var mp
          return ()
        Just _ -> return ()
    IntConst _ -> return ()
    IntVar v -> do
      mp <- get
      case M.lookup v mp of
        Nothing -> do
          var <- liftIO $ mkIntVar ctx (show v)
          put $ M.insert v var mp
          return ()
        Just _ -> return ()

boolVariableMap :: (Ord d, Show d) => Context -> BoolExpr d
                -> StateT (Map d AST) IO ()
boolVariableMap ctx expr =
  case expr of
    And es -> mapM_ (boolVariableMap ctx) es
    Or es -> mapM_ (boolVariableMap ctx) es
    Not e -> boolVariableMap ctx e
    Implies a b -> boolVariableMap ctx a >> boolVariableMap ctx b
    Comparison _ a b -> arithVariableMap ctx a >> arithVariableMap ctx b
    BoolConst _ -> return ()
    BoolVar v -> do
      mp <- get
      case M.lookup v mp of
        Nothing -> do
          var <- liftIO $ mkBoolVar ctx (show v)
          put $ M.insert v var mp
          return ()
        Just _ -> return ()

convertArithFormula :: Ord d => Context -> Map d AST -> ArithExpr d -> IO AST
convertArithFormula ctx vs expr =
  case expr of
    Add es -> if null es
                 then mkReal ctx 0 1
                 else mapM (convertArithFormula ctx vs) es >>= mkAdd ctx
    Mul es -> if null es
                 then mkReal ctx 1 1
                 else mapM (convertArithFormula ctx vs) es >>= mkMul ctx
    Sub a b -> do
      af <- convertArithFormula ctx vs a
      bf <- convertArithFormula ctx vs b
      mkSub ctx af bf
    Div a b -> do
      af <- convertArithFormula ctx vs a
      bf <- convertArithFormula ctx vs b
      mkDiv ctx af bf
    ToReal e -> convertArithFormula ctx vs e >>= mkToReal ctx
    RealConst n d -> mkReal ctx n d
    RealVar v -> return $ vs ! v
    IntConst i -> mkInt ctx i
    IntVar v -> return $ vs ! v

mkComparison :: Context -> CompType -> AST -> AST -> IO AST
mkComparison ctx op a b =
  case op of
    Lt -> mkLt ctx a b
    Le -> mkLe ctx a b
    Eq -> mkEq ctx a b
    Ge -> mkGe ctx a b
    Gt -> mkGt ctx a b
    Neq -> mkEq ctx a b >>= mkNot ctx

convertBoolFormula :: Ord d => Context -> Map d AST -> BoolExpr d -> IO AST
convertBoolFormula ctx vs expr =
  case expr of
    And es -> if null es
                 then mkBoolVal ctx True
                 else mapM (convertBoolFormula ctx vs) es >>= mkAnd ctx
    Or es -> if null es
                then mkBoolVal ctx False
                else mapM (convertBoolFormula ctx vs) es >>= mkOr ctx
    Not e -> convertBoolFormula ctx vs e >>= mkNot ctx
    Implies a b -> do
      af <- convertBoolFormula ctx vs a
      bf <- convertBoolFormula ctx vs b
      mkImplies ctx af bf
    Comparison op a b -> do
      af <- convertArithFormula ctx vs a
      bf <- convertArithFormula ctx vs b
      mkComparison ctx op af bf
    BoolConst b -> mkBoolVal ctx b
    BoolVar v -> return $ vs ! v

mkAssertion :: (Ord d, Show d) => Context -> BoolExpr d -> IO (AST, Map d AST)
mkAssertion ctx expr = do
  vars <- execStateT (boolVariableMap ctx expr) M.empty
  form <- convertBoolFormula ctx vars expr
  return (form, vars)

getModel :: Context -> Model -> Map d AST -> IO (Map d AST)
getModel ctx m = mapM (fmap snd .  \x -> modelEval ctx m x True)

checkAndGetModel :: (Ord d, Show d) => Context -> Solver -> BoolExpr d
                 -> IO (CheckResult d)
checkAndGetModel ctx s expr = do
  (f, vars) <- mkAssertion ctx expr
  solverAssert ctx s f
  res <- solverCheck ctx s
  case res of
    L_TRUE -> Sat <$> (solverGetModel ctx s >>= \m -> getModel ctx m vars)
    L_FALSE -> return Unsat
    L_UNDEF -> return Undef

getDoubles :: Context -> Map d AST -> IO (Maybe (Map d Double))
getDoubles ctx mp = Just <$> mapM (getNumeralDouble ctx) mp
