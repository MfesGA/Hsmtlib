module Examples(array, example1, example2) where

import           SMTLib2
import           SMTLib2.Int
import           SMTLib2.Array
import           SMTLib2.Core
import           Hsmtlib.HighLevel
import           Hsmtlib
import           Hsmtlib.Solver as Slv




pt :: GValResult -> IO ()
pt (GVUError str) = putStrLn str
pt _ = print "ola"


array :: IO ()
array = do
  solver <- startSolver Z3 Online "AUFLIA" Nothing Nothing
  produceModels solver
  declareFun solver (N "x") [] tInt
  declareFun solver (N "y") [] tInt
  declareFun solver (N "z") [] tInt
  declareFun solver (N "a1") [] $ tArray tInt tInt
  declareFun solver (N "a2") [] $ tArray tInt tInt
  declareFun solver (N "a3") [] $ tArray tInt tInt
  assert solver $ select (ct "a1") (ct "x") === (ct "x")
  assert solver $ store (ct "a1") (ct "x") (ct "y") ===  (ct "a1")
  checkSat solver >>= print
  getValue solver [ct "a1"] >>= print
  getValue solver [ct "a2 2"] >>= print
  getValue solver [ct "a3 1"] >>= print
  getValue solver [ct "x",ct "a2 2", ct "a1 1", ct "a3 2", ct "a2 4"]  >>= print
  exit solver
  print "fin"

example1 :: IO ()
example1 = do
  solver <- startSolver Z3 Online "AUFLIA" Nothing Nothing
  produceModels solver
  declareFun solver (N "a") [] tInt
  declareFun solver (N "f") [tInt, tBool] tInt
  assert solver $ nGeq (ct "a") 10
  assert solver $ nGeq (ct "(f a true)")  (literal 100)
  checkSat solver
  getValue solver [ct "a"] >>= print
  getValue solver [ct "(f 2 false)"] >>= print
  getValue solver [ct "(f 3 true)", ct "a"] >>= print
  exit solver
  print "fin"

example2 :: IO ()
example2 = do
  solver <- startSolver Z3 Online "QF_AUFLIA" Nothing Nothing
  produceModels solver
  declareFun solver (N "a") [] tInt
  declareFun solver (N "f") [tInt, tBool] tInt
  declareFun solver (N "a1") [] $ tArray tInt tInt
  declareFun solver (N "a2") [] $ tArray tInt tInt
  declareFun solver (N "a3") [] $ tArray tInt tInt
  assert solver $ nGeq (ct "a") 10
  assert solver $ nGeq (ct "(f a true)")  (literal 100)
  checkSat solver
  getValue solver [ct "a"] >>= pt
  getValue solver [ct "(select a1 a)"] >>= pt
  getValue solver [ct "(select a3 2)", ct "a", ct "(f a true)", ct "(select a1 a)"] >>= pt
  exit solver
  print "fin"


{--

main :: IO ()
main = do
  solver <- startSolver Z3 Online "QF_LIA"  Nothing Nothing
  setLogic solver (N "QF_LIA") >>= print
  declareFun solver (N "a") [] tInt >>= pt
  declareFun solver (N "x") [] tInt >>= pt
  declareFun solver (N "y") [] tInt >>= pt
  declareFun solver (N "f") [] tInt >>= pt
  getAssertions solver >>= pt
  checkSat solver >>= pt
  exit solver >>= pt
--}