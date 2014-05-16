{- |
Module      : Hsmtlib.Solver
  This module has most types,data and functions that a user might need to
  utilize the library.
-}
module Hsmtlib.Solver where

import           Data.Map
import           SMTLib2



-- | Logics that can be passed to the start of the solver
data Logic = AUFLIA
           | AUFLIRA
           | AUFNIRA
           | LRA
           | QF_ABV
           | QF_AUFBV
           | QF_AUFLIA
           | QF_AX
           | QF_BV
           | QF_IDL
           | QF_LIA
           | QF_LRA
           | QF_NIA
           | QF_NRA
           | QF_RDL
           | QF_UF
           | QF_UFBV
           | QF_UFIDL
           | QF_UFLIA
           | QF_UFLRA
           | QF_UFNRA
           | UFLRA
           | UFNIA
           deriving(Show)


-- | Solvers that are currently supported.
data Solvers = Z3 | Cvc4 | Yices | Mathsat | Altergo | Boolector


{- |
 Response from most commands that do not demand a feedback from the solver,
 e.g: setLogic,push,pop...
-}
data GenResult = Success -- ^ The command was successfully sent to the solver.
               | Unsupported -- ^ The solver does not support the command.
               | Error String -- ^ Some error occurred in the solver.
               -- | Some error occurred parsing the response from the solver.
               | GUError String
               deriving (Show,Eq)



-- | Response from the command  checkSat.
data SatResult = Sat -- ^ The solver has found a model.
               | Unsat -- ^ The solver has established there is no model.
               | Unknown -- ^ The search for a model was inconclusive.
               -- Some error occurred parsing the response from the solver.
               | SUError String
               deriving (Show)



-- | Response from the command  getValue
data GValResult = Res String Value -- ^ Name of the variable or function and result. 
                | VArrays Arrays -- ^ The result of arrays
                | Results [GValResult] -- ^ Multiple results from multiple requestes.
                | GVUError String -- ^ Some error occurred parsing the response from the solver.
                deriving (Show)


{- |
  When the value of an array or several values from diferent arrays are 
  requested with getValue then the value returned is a Map where the value
  ís the name of the array, and the value is also a map. This inner map has 
  as value the position of the array and returned value.
  Only integers are supported as values of arrays.
-}
type Arrays = Map String (Map String Integer)


-- |  The type returned by getValue on constants or functions.
data Value = VInt Integer
           | VRatio Rational
           | VBool Bool
           | VHex String
            deriving (Show)



-- | Avaliable modes to use a solver.
data Mode = Online | Script | Batch


{- |
  Alternative configuration of a solver which can be passed in the function
  startSolver in 'Main'
-}
data SolverConfig = Config 
                  { path :: String
                  , args :: [String]
                  }


-- | Solver data type that has all the functions.
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