module Sudoku where


import           SMTLib2
import           SMTLib2.Int
import           SMTLib2.Array
import           SMTLib2.Core
import           SMTLib2.BitVector 
import           Hsmtlib
import           Hsmtlib.HighLevel
import           Hsmtlib.Solver     as Slv
import           Hsmtlib.Solvers.Altergo   (startAltErgo)
import           Hsmtlib.Solvers.Boolector (startBoolector)
import           Hsmtlib.Solvers.Cvc4      (startCvc4)
import           Hsmtlib.Solvers.MathSAT   (startMathSat)
import           Hsmtlib.Solvers.Yices     (startYices)
import           Hsmtlib.Solvers.Z3        (startZ3)



--altergo Script ----

altScript:: IO()
altScript= do 
	solver <- startSolver Altergo Slv.Script QF_NIA Nothing (Just "te.smt2")
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
  	assertDistinct solver [ct  "x11", ct  "x12", ct  "x13", ct  "x14"] >>=print 
	assertDistinct solver [ct  "x21", ct  "x22", ct  "x23", ct  "x24"] >>=print 
	assertDistinct solver [ct  "x31", ct  "x32", ct  "x33", ct  "x34"] >>=print 
	assertDistinct solver [ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>=print 
	assertDistinct solver [ct  "x11", ct  "x21", ct  "x31", ct  "x41"] >>=print 
	assertDistinct solver [ct  "x12", ct  "x22", ct  "x32", ct  "x42"] >>=print
	assertDistinct solver [ct  "x13", ct  "x23", ct  "x33", ct  "x43"] >>=print
	assertDistinct solver [ct  "x14", ct  "x24", ct  "x34", ct  "x44"] >>=print
  	assertDistinct solver [ct  "x11", ct  "x12", ct  "x21", ct  "x22"] >>=print
	assertDistinct solver [ct  "x13", ct  "x14", ct  "x23", ct  "x24"] >>=print
	assertDistinct solver [ct  "x31", ct  "x32", ct  "x41", ct  "x42"] >>=print
	assertDistinct solver [ct  "x33", ct  "x34", ct  "x43", ct  "x44"] >>=print
	maping solver (nLt (lit 0)) [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print 
	maping solver (nGeq (lit 4)) [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print 
  	checkSat solver >>= print
	getValue solver [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print
	declType solver "edc" 5 >>= print
	exit solver >>= print 


---AltErgo Online----


altOnline:: IO()-- does not terminate 
altOnline= do 
	solver <- startSolver Altergo Slv.Online QF_NIA Nothing Nothing
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
  	assertDistinct solver [ct  "x11", ct  "x12", ct  "x13", ct  "x14"] >>=print 
	assertDistinct solver [ct  "x21", ct  "x22", ct  "x23", ct  "x24"] >>=print 
	assertDistinct solver [ct  "x31", ct  "x32", ct  "x33", ct  "x34"] >>=print 
	assertDistinct solver [ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>=print 
	assertDistinct solver [ct  "x11", ct  "x21", ct  "x31", ct  "x41"] >>=print 
	assertDistinct solver [ct  "x12", ct  "x22", ct  "x32", ct  "x42"] >>=print
	assertDistinct solver [ct  "x13", ct  "x23", ct  "x33", ct  "x43"] >>=print
	assertDistinct solver [ct  "x14", ct  "x24", ct  "x34", ct  "x44"] >>=print
  	assertDistinct solver [ct  "x11", ct  "x12", ct  "x21", ct  "x22"] >>=print
	assertDistinct solver [ct  "x13", ct  "x14", ct  "x23", ct  "x24"] >>=print
	assertDistinct solver [ct  "x31", ct  "x32", ct  "x41", ct  "x42"] >>=print
	assertDistinct solver [ct  "x33", ct  "x34", ct  "x43", ct  "x44"] >>=print
	maping solver (nLt (lit 0)) [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print 
	maping solver (nGeq (lit 4)) [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print 
  	checkSat solver >>= print
	getValue solver [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print
	declType solver "edc" 5 >>= print
	exit solver >>= print 

---Mathsat Online----


matOnline:: IO()
matOnline= do 
	solver <- startSolver Mathsat Slv.Online QF_NIA Nothing Nothing
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
  	assertDistinct solver [ct  "x11", ct  "x12", ct  "x13", ct  "x14"] >>=print 
	assertDistinct solver [ct  "x21", ct  "x22", ct  "x23", ct  "x24"] >>=print 
	assertDistinct solver [ct  "x31", ct  "x32", ct  "x33", ct  "x34"] >>=print 
	assertDistinct solver [ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>=print 
	assertDistinct solver [ct  "x11", ct  "x21", ct  "x31", ct  "x41"] >>=print 
	assertDistinct solver [ct  "x12", ct  "x22", ct  "x32", ct  "x42"] >>=print
	assertDistinct solver [ct  "x13", ct  "x23", ct  "x33", ct  "x43"] >>=print
	assertDistinct solver [ct  "x14", ct  "x24", ct  "x34", ct  "x44"] >>=print
  	assertDistinct solver [ct  "x11", ct  "x12", ct  "x21", ct  "x22"] >>=print
	assertDistinct solver [ct  "x13", ct  "x14", ct  "x23", ct  "x24"] >>=print
	assertDistinct solver [ct  "x31", ct  "x32", ct  "x41", ct  "x42"] >>=print
	assertDistinct solver [ct  "x33", ct  "x34", ct  "x43", ct  "x44"] >>=print
	maping solver (nLt (lit 0)) [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print 
	maping solver (nGeq (lit 4)) [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print 
  	checkSat solver >>= print
	getValue solver [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print
	declType solver "edc" 5 >>= print
	exit solver >>= print 

--mathsat Script ----

matScript:: IO()
matScript= do 
	solver <- startSolver Mathsat Slv.Script QF_NIA Nothing (Just "te.smt2")
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
  	assertDistinct solver [ct  "x11", ct  "x12", ct  "x13", ct  "x14"] >>=print 
	assertDistinct solver [ct  "x21", ct  "x22", ct  "x23", ct  "x24"] >>=print 
	assertDistinct solver [ct  "x31", ct  "x32", ct  "x33", ct  "x34"] >>=print 
	assertDistinct solver [ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>=print 
	assertDistinct solver [ct  "x11", ct  "x21", ct  "x31", ct  "x41"] >>=print 
	assertDistinct solver [ct  "x12", ct  "x22", ct  "x32", ct  "x42"] >>=print
	assertDistinct solver [ct  "x13", ct  "x23", ct  "x33", ct  "x43"] >>=print
	assertDistinct solver [ct  "x14", ct  "x24", ct  "x34", ct  "x44"] >>=print
  	assertDistinct solver [ct  "x11", ct  "x12", ct  "x21", ct  "x22"] >>=print
	assertDistinct solver [ct  "x13", ct  "x14", ct  "x23", ct  "x24"] >>=print
	assertDistinct solver [ct  "x31", ct  "x32", ct  "x41", ct  "x42"] >>=print
	assertDistinct solver [ct  "x33", ct  "x34", ct  "x43", ct  "x44"] >>=print
	maping solver (nLt (lit 0)) [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print 
	maping solver (nGeq (lit 4)) [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print 
  	checkSat solver >>= print
	getValue solver [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print
	declType solver "edc" 5 >>= print
	exit solver >>= print 

---Yices Online----


yiOnline:: IO()
yiOnline= do 
	solver <- startSolver Yices Slv.Online QF_NIA Nothing Nothing
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
  	assertDistinct solver [ct  "x11", ct  "x12", ct  "x13", ct  "x14"] >>=print 
	assertDistinct solver [ct  "x21", ct  "x22", ct  "x23", ct  "x24"] >>=print 
	assertDistinct solver [ct  "x31", ct  "x32", ct  "x33", ct  "x34"] >>=print 
	assertDistinct solver [ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>=print 
	assertDistinct solver [ct  "x11", ct  "x21", ct  "x31", ct  "x41"] >>=print 
	assertDistinct solver [ct  "x12", ct  "x22", ct  "x32", ct  "x42"] >>=print
	assertDistinct solver [ct  "x13", ct  "x23", ct  "x33", ct  "x43"] >>=print
	assertDistinct solver [ct  "x14", ct  "x24", ct  "x34", ct  "x44"] >>=print
  	assertDistinct solver [ct  "x11", ct  "x12", ct  "x21", ct  "x22"] >>=print
	assertDistinct solver [ct  "x13", ct  "x14", ct  "x23", ct  "x24"] >>=print
	assertDistinct solver [ct  "x31", ct  "x32", ct  "x41", ct  "x42"] >>=print
	assertDistinct solver [ct  "x33", ct  "x34", ct  "x43", ct  "x44"] >>=print
	maping solver (nLt (lit 0)) [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print 
	maping solver (nGeq (lit 4)) [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print 
  	checkSat solver >>= print
	getValue solver [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print
	declType solver "edc" 5 >>= print
	exit solver >>= print 

--Yices Script ----

yiScript:: IO()
yiScript= do 
	solver <- startSolver Yices Slv.Script QF_NIA Nothing (Just "te.smt2")
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
  	assertDistinct solver [ct  "x11", ct  "x12", ct  "x13", ct  "x14"] >>=print 
	assertDistinct solver [ct  "x21", ct  "x22", ct  "x23", ct  "x24"] >>=print 
	assertDistinct solver [ct  "x31", ct  "x32", ct  "x33", ct  "x34"] >>=print 
	assertDistinct solver [ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>=print 
	assertDistinct solver [ct  "x11", ct  "x21", ct  "x31", ct  "x41"] >>=print 
	assertDistinct solver [ct  "x12", ct  "x22", ct  "x32", ct  "x42"] >>=print
	assertDistinct solver [ct  "x13", ct  "x23", ct  "x33", ct  "x43"] >>=print
	assertDistinct solver [ct  "x14", ct  "x24", ct  "x34", ct  "x44"] >>=print
  	assertDistinct solver [ct  "x11", ct  "x12", ct  "x21", ct  "x22"] >>=print
	assertDistinct solver [ct  "x13", ct  "x14", ct  "x23", ct  "x24"] >>=print
	assertDistinct solver [ct  "x31", ct  "x32", ct  "x41", ct  "x42"] >>=print
	assertDistinct solver [ct  "x33", ct  "x34", ct  "x43", ct  "x44"] >>=print
	maping solver (nLt (lit 0)) [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print 
	maping solver (nGeq (lit 4)) [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print 
  	checkSat solver >>= print
	getValue solver [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print
	declType solver "edc" 5 >>= print
	exit solver >>= print 




---cvc4 Online----


c4Online:: IO()
c4Online= do 
	solver <- startSolver Cvc4 Slv.Online QF_NIA Nothing Nothing
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
  	assertDistinct solver [ct  "x11", ct  "x12", ct  "x13", ct  "x14"] >>=print 
	assertDistinct solver [ct  "x21", ct  "x22", ct  "x23", ct  "x24"] >>=print 
	assertDistinct solver [ct  "x31", ct  "x32", ct  "x33", ct  "x34"] >>=print 
	assertDistinct solver [ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>=print 
	assertDistinct solver [ct  "x11", ct  "x21", ct  "x31", ct  "x41"] >>=print 
	assertDistinct solver [ct  "x12", ct  "x22", ct  "x32", ct  "x42"] >>=print
	assertDistinct solver [ct  "x13", ct  "x23", ct  "x33", ct  "x43"] >>=print
	assertDistinct solver [ct  "x14", ct  "x24", ct  "x34", ct  "x44"] >>=print
  	assertDistinct solver [ct  "x11", ct  "x12", ct  "x21", ct  "x22"] >>=print
	assertDistinct solver [ct  "x13", ct  "x14", ct  "x23", ct  "x24"] >>=print
	assertDistinct solver [ct  "x31", ct  "x32", ct  "x41", ct  "x42"] >>=print
	assertDistinct solver [ct  "x33", ct  "x34", ct  "x43", ct  "x44"] >>=print
	maping solver (nLt (lit 0)) [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print 
	maping solver (nGeq (lit 4)) [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print 
  	checkSat solver >>= print
	getValue solver [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print
	declType solver "edc" 5 >>= print
	exit solver >>= print 

--CVC4 Script ----

c4Script:: IO()
c4Script= do 
	solver <- startSolver Cvc4 Slv.Script QF_NIA Nothing (Just "te.smt2")
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
  	assertDistinct solver [ct  "x11", ct  "x12", ct  "x13", ct  "x14"] >>=print 
	assertDistinct solver [ct  "x21", ct  "x22", ct  "x23", ct  "x24"] >>=print 
	assertDistinct solver [ct  "x31", ct  "x32", ct  "x33", ct  "x34"] >>=print 
	assertDistinct solver [ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>=print 
	assertDistinct solver [ct  "x11", ct  "x21", ct  "x31", ct  "x41"] >>=print 
	assertDistinct solver [ct  "x12", ct  "x22", ct  "x32", ct  "x42"] >>=print
	assertDistinct solver [ct  "x13", ct  "x23", ct  "x33", ct  "x43"] >>=print
	assertDistinct solver [ct  "x14", ct  "x24", ct  "x34", ct  "x44"] >>=print
  	assertDistinct solver [ct  "x11", ct  "x12", ct  "x21", ct  "x22"] >>=print
	assertDistinct solver [ct  "x13", ct  "x14", ct  "x23", ct  "x24"] >>=print
	assertDistinct solver [ct  "x31", ct  "x32", ct  "x41", ct  "x42"] >>=print
	assertDistinct solver [ct  "x33", ct  "x34", ct  "x43", ct  "x44"] >>=print
	maping solver (nLt (lit 0)) [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print 
	maping solver (nGeq (lit 4)) [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print 
  	checkSat solver >>= print
	getValue solver [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print
	declType solver "edc" 5 >>= print
	exit solver >>= print 

---Z3 Online----


z3Online:: IO()
z3Online= do 
	solver <- startSolver Z3 Slv.Online QF_NIA Nothing Nothing
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
  	assertDistinct solver [ct  "x11", ct  "x12", ct  "x13", ct  "x14"] >>=print 
	assertDistinct solver [ct  "x21", ct  "x22", ct  "x23", ct  "x24"] >>=print 
	assertDistinct solver [ct  "x31", ct  "x32", ct  "x33", ct  "x34"] >>=print 
	assertDistinct solver [ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>=print 
	assertDistinct solver [ct  "x11", ct  "x21", ct  "x31", ct  "x41"] >>=print 
	assertDistinct solver [ct  "x12", ct  "x22", ct  "x32", ct  "x42"] >>=print
	assertDistinct solver [ct  "x13", ct  "x23", ct  "x33", ct  "x43"] >>=print
	assertDistinct solver [ct  "x14", ct  "x24", ct  "x34", ct  "x44"] >>=print
  	assertDistinct solver [ct  "x11", ct  "x12", ct  "x21", ct  "x22"] >>=print
	assertDistinct solver [ct  "x13", ct  "x14", ct  "x23", ct  "x24"] >>=print
	assertDistinct solver [ct  "x31", ct  "x32", ct  "x41", ct  "x42"] >>=print
	assertDistinct solver [ct  "x33", ct  "x34", ct  "x43", ct  "x44"] >>=print
	maping solver (nLt (lit 0)) [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print 
	maping solver (nGeq (lit 4)) [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print 
  	checkSat solver >>= print
	getValue solver [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print
	declType solver "edc" 5 >>= print
	exit solver >>= print 

--Z3 Script ----

z3Script:: IO()
z3Script= do 
	solver <- startSolver Z3 Slv.Script QF_NIA Nothing (Just "te.smt2")
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
  	assertDistinct solver [ct  "x11", ct  "x12", ct  "x13", ct  "x14"] >>=print 
	assertDistinct solver [ct  "x21", ct  "x22", ct  "x23", ct  "x24"] >>=print 
	assertDistinct solver [ct  "x31", ct  "x32", ct  "x33", ct  "x34"] >>=print 
	assertDistinct solver [ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>=print 
	assertDistinct solver [ct  "x11", ct  "x21", ct  "x31", ct  "x41"] >>=print 
	assertDistinct solver [ct  "x12", ct  "x22", ct  "x32", ct  "x42"] >>=print
	assertDistinct solver [ct  "x13", ct  "x23", ct  "x33", ct  "x43"] >>=print
	assertDistinct solver [ct  "x14", ct  "x24", ct  "x34", ct  "x44"] >>=print
  	assertDistinct solver [ct  "x11", ct  "x12", ct  "x21", ct  "x22"] >>=print
	assertDistinct solver [ct  "x13", ct  "x14", ct  "x23", ct  "x24"] >>=print
	assertDistinct solver [ct  "x31", ct  "x32", ct  "x41", ct  "x42"] >>=print
	assertDistinct solver [ct  "x33", ct  "x34", ct  "x43", ct  "x44"] >>=print
	maping solver (nLt (lit 0)) [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print 
	maping solver (nGeq (lit 4)) [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print 
  	checkSat solver >>= print
	getValue solver [ct  "x11", ct  "x12", ct  "x13", ct  "x14", ct  "x21", ct  "x22", ct  "x23", ct  "x24", ct  "x31", ct  "x32", ct  "x33", ct  "x34", ct  "x41", ct  "x42", ct  "x43", ct  "x44"] >>= print
	declType solver "edc" 5 >>= print
	exit solver >>= print 


--BVexaple
testbv :: IO()
testbv = do 
	solver <- startSolver Boolector Slv.Script QF_BV Nothing (Just "te.smt2")
	declConst solver  "x" (tBitVec 64)
	declConst solver "y" (tBitVec 64)
	assert solver (SMTLib2.Core.not ((bvand (bvnot (ct  "x")) (bvnot (ct  "y"))) === (bvnot (bvor (ct  "x") (ct  "y"))))  )>>= print
	checkSat solver >>=print
	getValue solver [ct  "x", ct  "y", extract 0 1 (ct  "x")] >>= print
	exit solver >>=print


