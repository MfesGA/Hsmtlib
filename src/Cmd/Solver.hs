{- |
Module      : Solver
  This module has most types,data and functions that a user might need to
  utilize the library.
-}
module Cmd.Solver where

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
type Result = String

-- | Sovler's that are currently supported.
data Solvers = Z3 | Cvc4 | Yices | Mathsat | Altergo

-- | Avaliable modes to use a solver.
data Mode = Online | Context | Script


{- |
  Alternative configuration of a solver which can be passed in the function
  startSolver in 'Main'
-}
data SolverConfig =
    Config { path :: String
           , args :: [String]
           }


{-|
  The alternative Result given by a solver using the context Mode.
-}
data CtResult = CtxRes
              { context :: [String] -- ^ List with all the commands.
              , result :: Result -- ^ Result of the last command.
              }

instance Show CtResult where
    show =  head.lines.result


{-|
 Solver data type that has all the functions.
 There exists two Constructors Solver and CtSolver, Solver is used for Online
 and Script mode. CtSolver is only used in ContextMode.

 The types of both are similar the only diferenc is that in CtSolver we need
 too keep the list of all commands used, so each commands needs to receive
 the list of the last command.

 To facilitate this there existis some combinators further bellow,
-}
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
    , getValue      :: [Expr]-> IO Result
    , getProof      :: IO Result
    , getUnsatCore  :: IO Result
    , getInfo       :: InfoFlag -> IO Result
    , getOption     :: Name -> IO Result
    , exit          :: IO Result
    }
    | CtSolver
    { setLogicCt      :: Name -> IO CtResult -> IO CtResult
    , setOptionCt     :: Option -> IO CtResult -> IO CtResult
    , setInfoCt       :: Attr -> IO CtResult -> IO CtResult
    , declareTypeCt   :: Name -> Integer -> IO CtResult -> IO CtResult
    , defineTypeCt    :: Name -> [Name] -> Type -> IO CtResult -> IO CtResult
    , declareFunCt    :: Name -> [Type] -> Type -> IO CtResult -> IO CtResult
    , defineFunCt     :: Name -> [Binder] -> Type -> Expr -> IO CtResult -> IO CtResult
    , pushCt          :: Integer -> IO CtResult -> IO CtResult
    , popCt           :: Integer -> IO CtResult -> IO CtResult
    , assertCt        :: Expr -> IO CtResult -> IO CtResult
    , checkSatCt      :: IO CtResult -> IO CtResult
    , getAssertionsCt :: IO CtResult -> IO CtResult
    , getValueCt      :: [Expr] -> IO CtResult -> IO CtResult
    , getProofCt      :: IO CtResult -> IO CtResult
    , getUnsatCoreCt  :: IO CtResult -> IO CtResult
    , getInfoCt       :: InfoFlag -> IO CtResult -> IO CtResult
    , getOptionCt     :: Name -> IO CtResult -> IO CtResult
    , exitCt          :: IO CtResult -> IO CtResult
    }


{-|
  The following combinators are used in context mode to write cleaner code.

  The combinator |*| should be used only after the first command and  the other(|#|) should be used in between.

  e.g:
@
main :: IO ()
main = do
  solver <- startSolver Cvc4 Slv.Context "QF_LIA"  Nothing Nothing
  declareFunCt solver (N "a") [] tInt |*|
  declareFunCt solver (N "x") [] tInt |#|
  declareFunCt solver (N "y") [] tInt |#|
  checkSatCt solver |#|
  exitCt solver >>= print
@
-}


(|*|) :: (IO CtResult -> IO CtResult)
      -> (IO CtResult -> IO CtResult)
      -> IO CtResult
(|*|)  f g = g  (f  (return CtxRes {context = [], result = ""}))


(|#|) :: IO CtResult
      -> (IO CtResult -> IO CtResult)
      -> IO CtResult
(|#|) f g = g f




