module Sudoku where


import           SMTLib2
import           SMTLib2.Int
import           Hsmtlib
import           Hsmtlib.HighLevel
import           Hsmtlib.Solver     as Slv
import           Hsmtlib.Solvers.Altergo   (startAltErgo)
import           Hsmtlib.Solvers.Boolector (startBoolector)
import           Hsmtlib.Solvers.Cvc4      (startCvc4)
import           Hsmtlib.Solvers.MathSAT   (startMathSat)
import           Hsmtlib.Solvers.Yices     (startYices)
import           Hsmtlib.Solvers.Z3        (startZ3)


mapassert solver [] = do
	return () 

mapassert solver (a:as) = do 
	assert solver a 
	mapassert solver as
 
maping :: Solver -> (a -> Expr) -> [a] -> IO ()
maping solver expr a = mapassert solver  (map expr a) 

mapeia2 :: Solver -> (a -> Expr) -> [a] -> IO ([IO (Result)])
mapeia2 solver expr a = return $ map (assert solver) (map expr a)

testa:: IO()
testa = do 
	solver <- startZ3 Slv.Script "QF_AUFLIA" Nothing (Just "test.hs")
	declareFun solver (N "a") [] tInt >>= print
  	declareFun solver (N "x") [] tInt >>= print
  	declareFun solver (N "y") [] tInt >>= print
  	declareFun solver (N "f") [] tInt >>= print	
	maping solver (nGeq (literal 0)) [constant "a", constant "x"] >>= print 
	exit solver >>=print


cria:: IO()
cria = do 
	solver <- startZ3 Slv.Script "QF_NIA" Nothing (Just "te.smt2")
	setOption solver (OptProduceModels True)
	declareFun solver (N "x11") [] tInt 
	declareFun solver (N "x12") [] tInt 
	declareFun solver (N "x13") [] tInt
	declareFun solver (N "x14") [] tInt
	declareFun solver (N "x21") [] tInt 
	declareFun solver (N "x22") [] tInt 
	declareFun solver (N "x23") [] tInt
	declareFun solver (N "x24") [] tInt
	declareFun solver (N "x31") [] tInt 
	declareFun solver (N "x32") [] tInt 
	declareFun solver (N "x33") [] tInt
	declareFun solver (N "x34") [] tInt
	declareFun solver (N "x41") [] tInt 
	declareFun solver (N "x42") [] tInt 
	declareFun solver (N "x43") [] tInt
	declareFun solver (N "x44") [] tInt  
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


