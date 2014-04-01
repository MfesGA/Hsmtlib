module Sudoku where

import Z3
import           SMTLib2
import           SMTLib2.Int
import           Cmd.Solver  as Slv

import           Z3          (startZ3)

sayfun:: String -> Expr
sayfun s = (App (I (N s) []) (Nothing) [])

literal :: Integer -> Expr
literal a = (Lit(LitNum a)) 

cria:: IO(Result)
cria = do 
	solver <- startZ3 Slv.Script "QF_AUFLIA" Nothing (Just "test.hs")
	setOption solver (OptProduceModels True)
	declareFun solver (N "a") [] tInt 
	declareFun solver (N "b") [] tInt 
	declareFun solver (N "c") [] tInt
	declareFun solver (N "d") [] tInt 
	declareFun solver (N "e") [] tInt
		
	assert solver (nGt (sayfun("a")) ((nAdd (sayfun("b")) (literal 2 ))))
	assert solver (nLeq (sayfun("a")) (nAdd (nMul (literal 2) (sayfun("c"))) (literal 10)))
	assert solver (nGeq (sayfun("a")) (nAdd (nMul (literal 2) (sayfun("c"))) (literal 10)))
	assert solver (nLeq (nAdd (sayfun("c") ) (sayfun("b")) ) (literal 1000) )
	assert solver (nGt (sayfun("d")) (sayfun("e")))
	--------------------------------------------------------------------------------------------	
	--assert solver (nLt (sayfun("d")) (sayfun("e")))
	--assert solver (nGt (sayfun("d")) (literal 4))
	--assert solver (nLt (sayfun("d")) (literal 4))
	checkSat solver >>= print
 
	getValue solver [(sayfun("a")), (sayfun("b")), (sayfun("c")), (sayfun("d")), (sayfun("e"))]  





{-


-}


