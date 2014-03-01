module Main where

import Z3
import SmtConfig as Conf
import Solver
import SMTLib2
import SMTLib2.Core
import SMTLib2.Int
import System.IO


data Solvers = Z3 | Yices | Cvc4


startSolver :: Solvers -> String -> Mode -> Maybe Config-> Maybe FilePath -> IO Solver
startSolver Z3 = startZ3

main :: IO ()
main = do
  solver <- startSolver Z3 "QF_LIA" Online Nothing Nothing
  declareFun solver (N "a") [] tInt >>= print
  --(declareFun solver (N "f") [tInt,tBool] tBool) >>= print
  declareFun solver (N "x") [] tInt >>= print
  declareFun solver (N "y") [] tInt >>= print
 -- (assert solver $ app (I (N "x") [2])
  --(assert solver $ app I (N "x") [] === num (2 :: Integer)) >>= print
  --(assert solver $ app I (N "x") [] `nLt` app (N "y") []) >>= print
  checkSat solver >>= print
  exit solver >>= print

mainScript :: IO ()
mainScript = do
  solver <- startSolver Z3 "QF_LIA" Conf.Script Nothing (Just "teste.hs")
  declareFun solver (N "a") [] tInt >>= print
  --(declareFun solver (N "f") [tInt,tBool] tBool) >>= print
  declareFun solver (N "x") [] tInt >>= print
  declareFun solver (N "y") [] tInt >>= print
 -- (assert solver $ app (I (N "x") [2])
  --(assert solver $ app I (N "x") [] === num (2 :: Integer)) >>= print
  --(assert solver $ app I (N "x") [] `nLt` app (N "y") []) >>= print
  checkSat solver >>= print
  exit solver >>= print