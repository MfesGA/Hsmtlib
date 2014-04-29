module Hsmtlib.HighLevel where

import           Hsmtlib.Solver            as Slv
import           SMTLib2

{- |
this function hides the application of a function on the SMT syntax receives the name of the function and the args and gives the corresponding SMT2Lib syntax 
-}
functionArg :: Name -> [Expr] -> Expr
functionArg fun fargs = App (I fun []) Nothing fargs

{- |
this function hides the application of a constant function on the SMT syntax receives the name of the function and gives the corresponding SMT2Lib syntax, the function must be already declared using declareFun
-}
ct :: String -> Expr 
ct x = App (I (N x) []) Nothing []

{- |
this function hides the application of distinct on the SMT syntax receives the solver and a list of the expressions which must be distinct and  gives the corresponding SMT2Lib syntax 
-}
assertDistinct :: Solver -> [Expr] -> IO GenResult
assertDistinct solver aexp = assert solver (App (I (N "distinct") []) (Nothing) aexp)

{- |
this function hides Integers on the SMT syntax receives a integer and gives the corresponding SMT2Lib syntax 
-}
literal :: Integer -> Expr
literal a = (Lit(LitNum a)) 


{-|
	This function simplifies the command to set the option to Produce Models
-}
produceModels :: Solver -> IO GenResult
produceModels solver = setOption solver (OptProduceModels True)	


