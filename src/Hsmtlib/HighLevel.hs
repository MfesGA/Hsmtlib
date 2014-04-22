module Hsmtlib.HighLevel where

import           Hsmtlib.Solver            as Slv
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

{- |
this function allows the user to given a list of expressions make a assert of them giving the SMTLib2 syntax corespondant (auxiliary function for maping)
-}
mapassert :: Solver -> [Expr] -> IO ()
mapassert solver [] = do
	return () 

mapassert solver (a:as) = do 
	assert solver a 
	mapassert solver as
 {- |
  this function when giving a solver and a function that gives an Expr and a list of the input type of that function, asserts the map of expressions its particulary useful to say that some set variables are all for example greater than zero 
-}
maping :: Solver -> (a -> Expr) -> [a] -> IO ()
maping solver expr a = mapassert solver  (map expr a) 


