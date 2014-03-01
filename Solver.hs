module Solver where
{--|
  Module with just the Solver type and the methods to start a solver
--}

import SMTLib2
import SmtConfig

--Later one Sexp will be the parsing of a s-expression from the result of the solver 
type Sexp = String

--Error will be the string read if any string is read from the std_err
type Error = String

--Future type of reult when we change the Process.hs
--type Result = (Sexp,Maybe Error)
type Result = String

data Solver = Solver
            { setLogic :: Name -> IO Result
            , setOption :: Option -> IO Result
            , setInfo :: Attr -> IO Result
            , declareType :: Name -> Integer -> IO Result
            , defineType :: Name -> [Name] -> Type -> IO Result
            , declareFun :: Name -> [Type] -> Type -> IO Result
            , push :: Integer -> IO Result
            , pop :: Integer -> IO Result
            , assert :: Expr -> IO Result
            , checkSat :: IO Result
            , getAssertions :: IO Result
            , getValue :: [Expr]-> IO Result
            , getProof :: IO Result
            , getUnsatCore :: IO Result
            , getInfo :: InfoFlag -> IO Result
            , getOption :: Name -> IO Result
            , exit :: IO Result 
            }
 

