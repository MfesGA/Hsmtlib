{- |
  Module: Hsmtlib.HighLevel
  This Module provide auxiliar functions that simplify writing expressions 
  to send to the solver.
-}

module Hsmtlib.HighLevel where

import           Hsmtlib.Solver            as Slv
import           SMTLib2


{- |  function that hides the creatinf of the quantifier forall-}
forall :: [Binder] -> Expr -> Expr
forall = Quant Forall 

{- |  function that hides the creatinf of the quantifier exists-}
exists :: [Binder] -> Expr -> Expr
exists = Quant Exists

{- |
    This function hides the application of a function on the SMT syntax
    receives the name of the function and the args and gives the corresponding 
    SMT2Lib syntax.
-}
fun :: String -> [Expr] -> Expr
fun name = App (I (N name) []) Nothing


{- | 
     This function hides the application of a constant function on the
     SMT syntax receives the name of the function and gives the corresponding 
     SMT2Lib syntax, the function must be already declared using declareFun.
-}
ct :: String -> Expr 
ct x = App (I (N x) []) Nothing []

{- | 
    This function hides the application of distinct on the SMT syntax
    receives the solver and a list of the expressions which must be distinct and gives the corresponding SMT2Lib syntax.
-}
assertDistinct :: Solver -> [Expr] -> IO Result
assertDistinct solver dexp = 
    assert solver (App (I (N "distinct") []) Nothing dexp)

{- | 
    This function hides Integers on the SMT syntax receives a integer
     and gives the corresponding SMT2Lib syntax.
-}
lit :: Int -> Expr
lit a = Lit $ LitNum (read (show a) :: Integer)


{- | 
    This function allows the user to given a list of expressions make a 
    assert of them giving the SMTLib2 syntax corespondant (auxiliary function for maping).
-}
mapAssert :: Solver -> [Expr] -> IO ()
mapAssert _ [] = return () 
mapAssert solver (a:as) = assert solver a >>  mapAssert solver as

{- | 
    This function when giving a solver and a function that gives an Expr and a list of the input type of that function, asserts the map of expressions its particulary useful to say that some set variables are all for example greater than zero.
-}
maping :: Solver -> (a -> Expr) -> [a] -> IO ()
maping solver expr a = mapAssert solver  (map expr a) 

{- | 
    This function hides the name creation on the SMT syntax receives a string and gives the corresponding SMT2Lib syntax for declaring a function.
-}
declFun :: Solver -> String -> [Type] -> Type -> IO Result
declFun solver name = declareFun solver (N name)

defFun :: Solver -> String -> [Binder] -> Type -> Expr -> IO Result
defFun solver name = defineFun solver (N name)


-- | This function binds a name to a type, and returns a Binder.
bind :: String -> Type -> Binder
bind name = Bind (N name)

{- | 
    This function hides Constants implemnted as functions without arguments on the SMT syntax receives a String and a type  and gives the corresponding SMT2Lib syntax for declaring a constant function.
-}
declConst :: Solver -> String -> Type -> IO Result
declConst solver name = declareFun solver (N name) []

{- | 
    This function hides Constants implemnted as functions without arguments on the SMT syntax receives a String and a type  and gives the corresponding SMT2Lib syntax for declaring a constant function.
-}
mapDeclConst :: Solver -> [String] -> Type -> IO ()
mapDeclConst _ [] _ = return ()
mapDeclConst solver (x:xs) y = 
    declConst solver x y >> mapDeclConst solver xs y 


{- | 
    This function hides the way to access an array (Hammered version)
    on the SMT syntax receives a integer and gives the corresponding 
    SMT2Lib syntax.
-} 
getPos :: Show a => Solver -> String -> a -> IO Result
getPos solver arr pos= let name = arr ++ " " ++ show pos in  
		getValue solver [App (I (N name ) []) Nothing []]   

-- | This function hides the Name Type in the declare type comand  
declType :: Solver -> String -> Integer -> IO Result
declType sol name = declareType sol (N name)


{- | 
    This function simplifies the command to set the option
    to Produce Models.
-}
produceModels :: Solver -> IO Result
produceModels solver = setOption solver (OptProduceModels True) 


produceProofs :: Solver -> IO Result
produceProofs solver = setOption solver (OptProduceProofs True) 

-- | This function simplifies the command to set the option to Interactive Mode.
interactiveMode :: Solver -> IO Result
interactiveMode solver = setOption solver (OptInteractiveMode True) 

