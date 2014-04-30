{- |
Module      : Solver
  This module has most types,data and functions that a user might need to
  utilize the library.
-}
module Hsmtlib.Solver where

import           Data.Map
import           SMTLib2

{-|
 Placeholder type that later on will be changed to a more complex type.
-}
-- We will parse the result of a solver and return an s-expression.
type Sexp = String


{-|
 Placeholder type that later on will be changed to a more complex type.
-}
type Error = String -- We will change the error to a more informative type.


{-|
 Placeholder type that later on will be changed to a more complex type.
-}
{-
  We still aren't sure what should  be the correct result, maybe a tuple:
  (Sexp, [Error])
-}




data GenResult = Success 
               | Unsupported 
               | Error String 
               | GUError String 
               deriving (Show)

data SatResult = Sat 
               | Unsat 
               | Unknown 
               | SUError String
               deriving (Show)






type Arrays = Map String (Map String Integer)

data Value = VInt Integer
           | VRatio Rational
           | VBool Bool
           | VHex String
            deriving (Show)

data GValResult = Res Value
                | Fun String Integer
                | Var String Value
                | VArrays Arrays
                | Results [GValResult]
                | GVUError String
                deriving (Show)


-- | Sovler's that are currently supported.
data Solvers = Z3 | Cvc4 | Yices | Mathsat | Altergo | Boolector

-- | Avaliable modes to use a solver.
data Mode = Online | Script | Batch

{- |
  Alternative configuration of a solver which can be passed in the function
  startSolver in 'Main'
-}
data SolverConfig = Config { path :: String
                           , args :: [String]
                           }


{-|
 Solver data type that has all the functions.
-}
data Solver = Solver
    { setLogic      :: Name -> IO GenResult
    , setOption     :: Option -> IO GenResult
    , setInfo       :: Attr -> IO GenResult
    , declareType   :: Name -> Integer -> IO GenResult
    , defineType    :: Name -> [Name] -> Type -> IO GenResult
    , declareFun    :: Name -> [Type] -> Type -> IO GenResult
    , defineFun     :: Name -> [Binder] -> Type -> Expr -> IO GenResult
    , push          :: Integer -> IO GenResult
    , pop           :: Integer -> IO GenResult
    , assert        :: Expr -> IO GenResult
    , checkSat      :: IO SatResult
    , getAssertions :: IO String
    , getValue      :: [Expr] -> IO GValResult
    , getProof      :: IO String
    , getUnsatCore  :: IO String
    , getInfo       :: InfoFlag -> IO String
    , getOption     :: Name -> IO String
    , exit          :: IO String
    }
    | BSolver
    { executeBatch :: [Command] -> IO String }





