module Hsmtlib.HighLevel where



import           Hsmtlib.Solver            as Slv
import           Hsmtlib.Solvers.Altergo   (startAltErgo)
import           Hsmtlib.Solvers.Boolector (startBoolector)
import           Hsmtlib.Solvers.Cvc4      (startCvc4)
import           Hsmtlib.Solvers.MathSAT   (startMathSat)
import           Hsmtlib.Solvers.Yices     (startYices)
import           Hsmtlib.Solvers.Z3        (startZ3)
import           SMTLib2
import           SMTLib2.Int

{- |
this function hides the application of a function on the SMT syntax receives the name of the function and the args and gives the corresponding SMT2Lib syntax 
-}
functionArg :: Name -> [Expr] -> Expr
functionArg fun args = (App (I fun []) (Nothing) args)

{- |
this function hides the application of a constant function on the SMT syntax receives the name of the function and gives the corresponding SMT2Lib syntax, the function must be already declared using declareFun
-}
constant :: String -> Expr 
constant x =(App (I (N x) []) (Nothing) [])

{- |
this function hides the application of distinct on the SMT syntax receives the solver and a list of the expressions which must be distinct and  gives the corresponding SMT2Lib syntax 
-}
assertDistinct :: Solver -> [Expr] -> IO(Result) 
assertDistinct solver exp = assert solver (App (I (N "distinct") []) (Nothing) exp)

{- |
this function hides Integers on the SMT syntax receives a integer and gives the corresponding SMT2Lib syntax 
-}
literal :: Integer -> Expr
literal a = (Lit(LitNum a)) 



