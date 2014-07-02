{- |
Module      : Hsmtlib.Solver
  This module has most types,data and functions that a user might need to
  utilize the library.
-}
module Hsmtlib.Solver where

import           Data.Map
import           SMTLib2
import           Hsmtlib.Parsers.Syntax hiding(Option, Command)


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





{-| Each response correspondes to it's respective command,
   for example CGI correspondes to the command Get Info.
   The Command CGR is the result for the command that dont return anything by default, for
   example push or pop. What is returned by this command is if it was succefull or not.
  
-}
data Result = CGR GenResponse 
            | CGI GetInfoResponse
            | CCS CheckSatResponse
            | CGAssert GetAssertion
            | CGP GetProofResponse
            | CGUC GetUnsatCoreResponse
            | CGV [GValResult]
            | CGAssig GetAssignmentResponse
            | CGO GetOptionResponse
            | ComError String -- ^ Error comunicating with the smt or Parsing
            deriving (Show, Eq)

-- |  Name of the variable or function and result 
data GValResult = Res String Value  
                -- | The result of arrays
                | VArrays Arrays 
                -- |  Multiple results from multiple requestes.
                | Results [GValResult] 
                {-|
                   In case it can't turn the result 
                   to one of the results above,
                   it return the syntax tree
                -}
                | Synt GetValueResponse 
                deriving (Show, Eq)


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
            deriving (Show, Eq)



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
    { setLogic      :: Name -> IO Result
    , setOption     :: Option -> IO Result
    , setInfo       :: Attr -> IO Result
    , declareType   :: Name -> Integer -> IO Result
    , defineType    :: Name -> [Name] -> Type -> IO Result
    , declareFun    :: Name -> [Type] -> Type -> IO Result
    , defineFun     :: Name -> [Binder] -> Type -> Expr -> IO Result
    , push          :: Integer -> IO Result
    , pop           :: Integer -> IO Result
    , assert        :: Expr -> IO Result
    , checkSat      :: IO Result
    , getAssertions :: IO Result
    , getValue      :: [Expr] -> IO Result
    , getProof      :: IO Result
    , getUnsatCore  :: IO Result
    , getInfo       :: InfoFlag -> IO Result
    , getOption     :: Name -> IO Result
    , exit          :: IO Result
    }
    | BSolver
    { executeBatch :: [Command] -> IO String }
