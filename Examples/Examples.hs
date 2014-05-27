qmodule Examples(testbv,example2,example1) where

import           Hsmtlib
import           Hsmtlib.HighLevel
import           Hsmtlib.Solver    as Slv
import           SMTLib2
import           SMTLib2.Array
import           SMTLib2.BitVector
import           SMTLib2.Core
import           SMTLib2.Int



pt :: GValResult -> IO ()
pt (GVUError str) = putStrLn str
pt x = print x 

{--
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
  assert solver $ seleconstant (constant "a1") (constant "x") === (constant "x")
  assert solver $ store (constant "a1") (constant "x") (constant "y") ===  (constant "a1")
  checkSat solver >>= print
  getValue solver [constant "a1"] >>= print
  getValue solver [constant "a2 2"] >>= print
  getValue solver [constant "a3 1"] >>= print
  getValue solver [constant "x",constant "a2 2", constant "a1 1", constant "a3 2", constant "a2 4"]  >>= print
  exit solver
  print "fin"
-}

example1 :: IO ()
example1 = do
  solver <- startSolver Z3 Online "AUFLIA" Nothing Nothing
  produceModels solver
  declareFun solver (N "a") [] tBool
  declareFun solver (N "f") [tInt, tBool] tInt
  assert solver $ nGeq (constant "a") 10
  assert solver $ nGeq (constant "(f a true)")  (literal 100)
  checkSat solver
  getValue solver [constant "a"] >>= print
  getValue solver [constant "(f 2 false)"] >>= print
  getValue solver [constant "(f 3 true)", constant "a"] >>= print
  exit solver
  print "fin"

example2 :: IO ()
example2 = do
  solver <- startSolver Cvc4 Online "QF_AUFLIA" Nothing Nothing
  produceModels solver
  declareFun solver (N "a") [] tInt
  declareFun solver (N "f") [tInt, tBool] tInt
  declareFun solver (N "g") [tInt] tBool
  declareFun solver (N "a1") [] $ tArray tInt tInt
  declareFun solver (N "a2") [] $ tArray tInt tInt
  declareFun solver (N "a3") [] $ tArray tInt tInt
  assert solver $ nGeq (constant "a") 10
  assert solver $ nGeq (constant "(f a true)")  (literal 100)
  checkSat solver
  getValue solver [constant "a"]
  getValue solver [constant "(select a1 a)"] >>= pt
  getValue solver [constant "(f a true)"] >>= pt
  getValue solver [constant "(select a3 2)", constant "a", constant "(f a true)", constant "(select a1 a)", constant"(g 2)"] >>= print
  exit solver
  print "fin"


testbv :: IO()
testbv = do
  solver <- startSolver Z3 Online "QF_BV" Nothing (Just "te.smt2")
  produceModels solver
  declConst solver  "x" (tBitVec 64)
  declConst solver "y" (tBitVec 64)
  assert solver bvand (bvnot (constant "x")) (bvnot (constant "y") === bvnot (bvor (constant "x") (constant "y"))) >>= print
  checkSat solver 
  getValue solver [constant "x", constant "y"] >>= print
  exit solver >>= print
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
