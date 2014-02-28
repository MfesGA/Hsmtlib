module Main where

import Z3
import SmtConfig
import Solver
import SMTLib2
import SMTLib2.Core
import SMTLib2.Int



data Solvers = Z3 | Yices | Cvc4


startSolver :: Solvers -> String -> Maybe Config-> IO Solver
startSolver Z3 logic = startZ3 logic

main :: IO ()
main = do
  solver <- startSolver Z3 "QF_LIA" Nothing
  (declareFun solver (N "a") [] tInt )>>= print
  (declareFun solver (N "f") [tInt,tBool] tBool) >>= print
  (declareFun solver (N "x") [] tInt) >>= print
  (declareFun solver (N "y") [] tInt) >>= print
 -- (assert solver $ app (I (N "x") [2])
  --(assert solver $ app I (N "x") [] === num (2 :: Integer)) >>= print
  --(assert solver $ app I (N "x") [] `nLt` app (N "y") []) >>= print
  (checkSat solver) >>= print