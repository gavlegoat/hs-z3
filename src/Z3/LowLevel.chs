#include <z3.h>

module Z3.LowLevel (
    Config
  , Context
  , LBool(..)
  , SortKind(..)
  , Solver
  , Model
  , Tactic
  , AST
  , mkConfig
  , delConfig
  , mkContext
  , mkTactic
  , mkSolver
  , solverAssert
  , solverCheck
  , solverGetModel
  , modelEval
  , getNumeralDouble
  , getSort
  , getSortKind
  , isAlgebraicNumber
  , getNumeralInt
  , getNumeralRationalInt64
  , getBoolValue
  , mkDefaultContext
  , mkSolverFromTactic
  , mkIntVar
  , mkBoolVar
  , mkRealVar
  , mkAnd
  , mkOr
  , mkNot
  , mkImplies
  , mkAdd
  , mkMul
  , mkSub
  , mkDiv
  , mkTrue
  , mkFalse
  , mkBool
  , mkLt
  , mkLe
  , mkEq
  , mkGe
  , mkGt
  , mkToReal
  , mkReal
  , mkInt
  , mkDouble
  ) where

import Foreign.Marshal (withArray, alloca)
import Foreign.Storable (Storable, peek)
import Foreign.Ptr (Ptr)

{# context lib = "z3" prefix = "Z3" #}

peekInt :: (Storable a, Integral a) => Ptr a -> IO Int
peekInt = fmap fromIntegral . peek

-- Types
{# pointer config as ^ foreign finalizer del_config as delConfig newtype #}
{# pointer Z3_context as Context foreign finalizer del_context as delContext newtype #}
{# pointer solver as ^ foreign finalizer solver_dec_ref as solverDecRef newtype #}
{# pointer tactic as ^ foreign finalizer tactic_dec_ref as tacticDecRef newtype #}
{# pointer symbol as ^ newtype #}
{# pointer sort as ^ newtype #}
{# pointer ast as AST #}
{# pointer model as ^ foreign finalizer model_dec_ref as modelDecRef newtype #}
{# enum lbool as LBool {} deriving(Eq) #}
{# enum sort_kind as ^ {} deriving(Eq, Show) #}

-- Functions for generating Z3 objects
{# fun unsafe mk_config as ^ {} -> `Config' #}
{# fun unsafe mk_context as ^ {`Config'} -> `Context' #}
{# fun unsafe mk_tactic as mkTacticInternal {`Context', `String'} -> `Tactic' #}
{# fun unsafe mk_solver as mkSolverInternal {`Context'} -> `Solver' #}
{# fun unsafe solver_inc_ref as ^ {`Context', `Solver'} -> `()' #}
{# fun unsafe tactic_inc_ref as ^ {`Context', `Tactic'} -> `()' #}
{# fun unsafe mk_solver_from_tactic as fromTacticInternal {`Context', `Tactic'} -> `Solver' #}
{# fun unsafe set_param_value as ^ {`Config', `String', `String'} -> `()' #}
{# fun unsafe model_inc_ref as ^ {`Context', `Model'} -> `()' #}

-- Solver interface
{# fun unsafe solver_assert as ^ {`Context', `Solver', `AST'} -> `()' #}
{# fun unsafe solver_check as ^ {`Context', `Solver'} -> `LBool' #}
{# fun unsafe solver_get_model as getModelInternal {`Context', `Solver'} -> `Model' #}

-- Model interface
{# fun unsafe model_eval as ^ {`Context', `Model', `AST', `Bool', alloca- `AST' peek*} -> `Bool' #}
{# fun unsafe get_numeral_double as ^ {`Context', `AST'} -> `Double' #}
{# fun unsafe get_sort as ^ {`Context', `AST'} -> `Sort' #}
{# fun unsafe get_sort_kind as ^ {`Context', `Sort'} -> `SortKind' #}
{# fun unsafe get_bool_value as ^ {`Context', `AST'} -> `LBool' #}
{# fun unsafe get_numeral_int as ^ {`Context', `AST', alloca- `Int' peekInt*} -> `Bool' #}
{# fun unsafe is_algebraic_number as ^ {`Context', `AST'} -> `Bool' #}
{# fun unsafe get_numeral_rational_int64 as ^
  {`Context', `AST', alloca- `Int' peekInt*, alloca- `Int' peekInt*} -> `Bool' #}

-- Building symbols and constants
{# fun unsafe mk_const as ^ {`Context', `Symbol', `Sort'} -> `AST' #}
{# fun unsafe mk_fresh_const as ^ {`Context', `String', `Sort'} -> `AST' #}
{# fun unsafe mk_string_symbol as ^ {`Context', `String'} -> `Symbol' #}
{# fun unsafe mk_int_sort as ^ {`Context'} -> `Sort' #}
{# fun unsafe mk_bool_sort as ^ {`Context'} -> `Sort' #}
{# fun unsafe mk_real_sort as ^ {`Context'} -> `Sort' #}

-- Functions for building formulas
{# fun unsafe mk_and as mkAndInternal {`Context', `Int', withArray* `[AST]'} -> `AST' #}
{# fun unsafe mk_or as mkOrInternal {`Context', `Int', withArray* `[AST]'} -> `AST' #}
{# fun unsafe mk_not as ^ {`Context', `AST'} -> `AST' #}
{# fun unsafe mk_implies as ^ {`Context', `AST', `AST'} -> `AST' #}
{# fun unsafe mk_add as mkAddInternal {`Context', `Int', withArray* `[AST]'} -> `AST' #}
{# fun unsafe mk_mul as mkMulInternal {`Context', `Int', withArray* `[AST]'} -> `AST' #}
{# fun unsafe mk_sub as mkSubInternal {`Context', `Int', withArray* `[AST]'} -> `AST' #}
{# fun unsafe mk_div as ^ {`Context', `AST', `AST'} -> `AST' #}
{# fun unsafe mk_true as ^ {`Context'} -> `AST' #}
{# fun unsafe mk_false as ^ {`Context'} -> `AST' #}
{# fun unsafe mk_lt as ^ {`Context', `AST', `AST'} -> `AST' #}
{# fun unsafe mk_le as ^ {`Context', `AST', `AST'} -> `AST' #}
{# fun unsafe mk_eq as ^ {`Context', `AST', `AST'} -> `AST' #}
{# fun unsafe mk_ge as ^ {`Context', `AST', `AST'} -> `AST' #}
{# fun unsafe mk_gt as ^ {`Context', `AST', `AST'} -> `AST' #}
{# fun unsafe mk_int2real as mkToReal {`Context', `AST'} -> `AST' #}
{# fun unsafe mk_real as ^ {`Context', `Int', `Int'} -> `AST' #}
{# fun unsafe mk_numeral as ^ {`Context', `String', `Sort'} -> `AST' #}
{# fun unsafe mk_int as mkIntInternal {`Context', `Int', `Sort'} -> `AST' #}

mkDefaultContext :: IO Context
mkDefaultContext = do
  cfg <- mkConfig
  setParamValue cfg "model" "true"
  ctx <- mkContext cfg
  return ctx

mkSolver :: Context -> IO Solver
mkSolver ctx = do
  s <- mkSolverInternal ctx
  solverIncRef ctx s
  return s

solverGetModel :: Context -> Solver -> IO Model
solverGetModel ctx s = do
  m <- getModelInternal ctx s
  modelIncRef ctx m
  return m

mkTactic :: Context -> String -> IO Tactic
mkTactic ctx str = do
  t <- mkTacticInternal ctx str
  tacticIncRef ctx t
  return t

mkSolverFromTactic :: Context -> Tactic -> IO Solver
mkSolverFromTactic ctx tact = do
  s <- fromTacticInternal ctx tact
  solverIncRef ctx s
  return s

mkIntVar :: Context -> String -> IO AST
mkIntVar ctx str = do
  i <- mkIntSort ctx
  mkFreshConst ctx str i

mkRealVar :: Context -> String -> IO AST
mkRealVar ctx str = do
  r <- mkRealSort ctx
  mkFreshConst ctx str r

mkBoolVar :: Context -> String -> IO AST
mkBoolVar ctx str = do
  b <- mkBoolSort ctx
  mkFreshConst ctx str b

mkAnd :: Context -> [AST] -> IO AST
mkAnd ctx es = mkAndInternal ctx (length es) es

mkOr :: Context -> [AST] -> IO AST
mkOr ctx es = mkOrInternal ctx (length es) es

mkAdd :: Context -> [AST] -> IO AST
mkAdd ctx es = mkAddInternal ctx (length es) es

mkMul :: Context -> [AST] -> IO AST
mkMul ctx es = mkMulInternal ctx (length es) es

mkSub :: Context -> AST -> AST -> IO AST
mkSub ctx a b = mkSubInternal ctx 2 [a, b]

mkBool :: Context -> Bool -> IO AST
mkBool ctx b = if b then mkTrue ctx else mkFalse ctx

mkInt :: Context -> Int -> IO AST
mkInt ctx i = do
  is <- mkIntSort ctx
  mkIntInternal ctx i is

mkDouble :: Context -> Double -> IO AST
mkDouble ctx d = do
  rs <- mkRealSort ctx
  mkNumeral ctx (show d) rs
