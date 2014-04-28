module Sudoku where


import           SMTLib2
import           SMTLib2.Int
import           SMTLib2.Array
import           SMTLib2.Core
import           Hsmtlib
import           Hsmtlib.HighLevel
import           Hsmtlib.Solver     as Slv
import           Hsmtlib.Solvers.Altergo   (startAltErgo)
import           Hsmtlib.Solvers.Boolector (startBoolector)
import           Hsmtlib.Solvers.Cvc4      (startCvc4)
import           Hsmtlib.Solvers.MathSAT   (startMathSat)
import           Hsmtlib.Solvers.Yices     (startYices)
import           Hsmtlib.Solvers.Z3        (startZ3)

ct a = constant a




mapeia2 :: Solver -> (a -> Expr) -> [a] -> IO ([IO (Result)])
mapeia2 solver expr a = return $ map (assert solver) (map expr a)



cria:: IO()
cria = do 
	solver <- startYices Slv.Script "QF_NIA" Nothing (Just "te.smt2")
	setOption solver (OptProduceModels True)
	declConst solver "x11" tInt 
	declConst solver "x12" tInt 
	declConst solver "x13" tInt
	declConst solver "x14" tInt
	declConst solver "x21" tInt 
	declConst solver "x22" tInt 
	declConst solver "x23" tInt
	declConst solver "x24" tInt
	declConst solver "x31" tInt 
	declConst solver "x32" tInt 
	declConst solver "x33" tInt
	declConst solver "x34" tInt
	declConst solver "x41" tInt 
	declConst solver "x42" tInt 
	declConst solver "x43" tInt
	declConst solver "x44" tInt  
  	assertDistinct solver [constant "x11", constant "x12", constant "x13", constant "x14"] >>=print 
	assertDistinct solver [constant "x21", constant "x22", constant "x23", constant "x24"] >>=print 
	assertDistinct solver [constant "x31", constant "x32", constant "x33", constant "x34"] >>=print 
	assertDistinct solver [constant "x41", constant "x42", constant "x43", constant "x44"] >>=print 
	assertDistinct solver [constant "x11", constant "x21", constant "x31", constant "x41"] >>=print 
	assertDistinct solver [constant "x12", constant "x22", constant "x32", constant "x42"] >>=print
	assertDistinct solver [constant "x13", constant "x23", constant "x33", constant "x43"] >>=print
	assertDistinct solver [constant "x14", constant "x24", constant "x34", constant "x44"] >>=print
  	assertDistinct solver [constant "x11", constant "x12", constant "x21", constant "x22"] >>=print
	assertDistinct solver [constant "x13", constant "x14", constant "x23", constant "x24"] >>=print
	assertDistinct solver [constant "x31", constant "x32", constant "x41", constant "x42"] >>=print
	assertDistinct solver [constant "x33", constant "x34", constant "x43", constant "x44"] >>=print
	maping solver (nLt (literal 0)) [constant "x11", constant "x12", constant "x13", constant "x14", constant "x21", constant "x22", constant "x23", constant "x24", constant "x31", constant "x32", constant "x33", constant "x34", constant "x41", constant "x42", constant "x43", constant "x44"] >>= print 
	maping solver (nGeq (literal 4)) [constant "x11", constant "x12", constant "x13", constant "x14", constant "x21", constant "x22", constant "x23", constant "x24", constant "x31", constant "x32", constant "x33", constant "x34", constant "x41", constant "x42", constant "x43", constant "x44"] >>= print 
  	checkSat solver >>= print
	getValue solver [constant "x11", constant "x12", constant "x13", constant "x14", constant "x21", constant "x22", constant "x23", constant "x24", constant "x31", constant "x32", constant "x33", constant "x34", constant "x41", constant "x42", constant "x43", constant "x44"] >>= print
	exit solver >>= print 


