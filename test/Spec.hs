import Z3
import Z3.LowLevel

form :: BoolExpr Int
form = And [Or [BoolVar 0, BoolVar 1], Implies (BoolVar 1) (BoolVar 0)]

main :: IO ()
main = do
  ctx <- mkDefaultContext
  solver <- mkSolver ctx
  (ast, vs) <- mkAssertion ctx form
  solverAssert ctx solver ast
  res <- solverCheck ctx solver
  case res of
    L_TRUE -> do
      print "Satisfiable"
      m <- solverGetModel ctx solver
      getModel ctx m vs >>= print
    L_FALSE -> print "Unsatisfiable"
    L_UNDEF -> print "Unknown"
