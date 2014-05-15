module Hsmtlib.HighLevel where

import           Hsmtlib.Solver            as Slv
import           SMTLib2



{- | This function hides the application of a function on the SMT syntax
    receives the name of the function and the args and gives the corresponding 
    SMT2Lib syntax.
-}
functionArg :: Name -> [Expr] -> Expr
functionArg fun fargs = App (I fun []) (Nothing) fargs


{- | This function hides the application of a constant function on the
     SMT syntax receives the name of the function and gives the corresponding 
     SMT2Lib syntax, the function must be already declared using declareFun.
-}
constant :: String -> Expr 
constant x =(App (I (N x) []) (Nothing) [])

{- | This function hides the application of distinct on the SMT syntax receives
     the solver and a list of the expressions which must be distinct and 
     gives the corresponding SMT2Lib syntax.
-}
assertDistinct :: Solver -> [Expr] -> IO( GenResult) 
assertDistinct solver dexp = 
    assert solver (App (I (N "distinct") []) (Nothing) dexp)

{- | This function hides Integers on the SMT syntax receives a integer
     and gives the corresponding SMT2Lib syntax.
-}
literal :: Int -> Expr
literal a = Lit $ LitNum (read (show a) :: Integer)


mapDeclConst :: Solver -> [String] -> Type -> IO ()
mapDeclConst _ [] _ = return ()
mapDeclConst solver (x:xs) y = 
    declConst solver x y >> mapDeclConst solver xs y 

{- | This function allows the user to given a list of expressions make a 
     assert of them giving the SMTLib2 syntax corespondant 
     (auxiliary function for maping).
-}
mapAssert :: Solver -> [Expr] -> IO ()
mapAssert _ [] = return () 
mapAssert solver (a:as) = assert solver a >>  mapAssert solver as

{- | This function when giving a solver and a function that gives an Expr and a
     list of the input type of that function, asserts the map of expressions 
     its particulary useful to say that some set variables are all 
     for example greater than zero.
-}
maping :: Solver -> (a -> Expr) -> [a] -> IO ()
maping solver expr a = mapAssert solver  (map expr a) 

{- | This function hides the name creation on the SMT syntax receives a string
     and gives the corresponding SMT2Lib syntax for declaring a function.
-}
declFun :: Solver -> String -> [Type] -> Type -> IO GenResult
declFun solver name dargs tipe = declareFun solver (N name) dargs tipe

defFun :: Solver -> String -> [Binder] -> Type -> Expr -> IO GenResult
defFun solver name binders result expr = 
    defineFun solver (N name) binders result expr

{- | This function hides Constants implemnted as functions without arguments 
     on the SMT syntax receives a String and a type  and gives the 
     corresponding SMT2Lib syntax for declaring a constant function.
-}
declConst :: Solver -> String -> Type -> IO GenResult
declConst solver name tipe = declareFun solver (N name ) [] tipe

{- | This function hides Constants implemnted as functions without arguments 
     on the SMT syntax receives a String and a type  and gives the 
     corresponding SMT2Lib syntax for declaring a constant function.
-}
mapDeclConst :: Solver -> [String] -> [Type] -> IO GenResult
mapDeclConst solver (x:xs) (y:ys)= do 
	declConst solver x y 
	mapDeclConst solver xs ys 



{- | This function hides the way to access an array (Hammered version)
     on the SMT syntax receives a integer and gives the corresponding 
     SMT2Lib syntax.
-} 
getPos :: Show a => Solver -> [Char] -> a -> IO GValResult
getPos solver arr pos= let name = arr ++" " ++show (pos) in  
		getValue solver [App (I (N name ) []) (Nothing) []]   

{- |
this function hides the Name Type in the declare type comand  
-}
declType :: Solver-> String ->Integer -> IO(GenResult)
declType sol name int = declareType sol (N name) int


{-|
    This function simplifies the command to set the option to Produce Models
-}
produceModels :: Solver -> IO GenResult
produceModels solver = setOption solver (OptProduceModels True) 


interactiveMode :: Solver -> IO GenResult
interactiveMode solver = setOption solver (OptInteractiveMode True) 
