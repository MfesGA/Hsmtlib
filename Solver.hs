module Solver where
{--|
  Module with just the Solver type and the methods to start a solver
--}

import           SMTLib2
import           Control.Monad
import           ContextCmd        (ContextArgs)

--Later one Sexp will be the parsing of a s-expression from the result of the solver
type Sexp = String

--Error will be the string read if any string is read from the std_err
type Error = String

--Future type of result when we change the Process.hs
--type Result = (Sexp,Maybe Error)
type Result = String

type CtResult=(IO Result,ContextArgs)

data Solver = Solver
            { setLogic      :: Name -> IO Result
            , setOption     :: Option -> IO Result
            , setInfo       :: Attr -> IO Result
            , declareType   :: Name -> Integer -> IO Result
            , defineType    :: Name -> [Name] -> Type -> IO Result
            , declareFun    :: Name -> [Type] -> Type -> IO Result
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
            { setLogicCt      ::  Name ->  ContextArgs ->CtResult
            , setOptionCt     ::  Option ->  ContextArgs ->CtResult
            , setInfoCt       ::  Attr ->  ContextArgs ->CtResult
            , declareTypeCt   ::  Name -> Integer ->ContextArgs ->  CtResult
            , defineTypeCt    ::  Name -> [Name] -> Type ->ContextArgs ->  CtResult
            , declareFunCt    ::  Name -> [Type] -> Type -> ContextArgs -> CtResult
            , pushCt          ::  Integer ->  ContextArgs ->CtResult
            , popCt           ::  Integer -> ContextArgs -> CtResult
            , assertCt        ::  Expr -> ContextArgs -> CtResult
            , checkSatCt      ::  ContextArgs -> CtResult
            , getAssertionsCt ::  ContextArgs -> CtResult
            , getValueCt      ::  [Expr]->  ContextArgs ->CtResult
            , getProofCt      ::  ContextArgs -> CtResult
            , getUnsatCoreCt  ::  ContextArgs -> CtResult
            , getInfoCt       :: InfoFlag ->   ContextArgs ->CtResult
            , getOptionCt     ::  Name -> ContextArgs -> CtResult
            , exitCt          ::  ContextArgs -> CtResult
            }



