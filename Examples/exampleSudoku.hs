module Sudoku where

import Z3
import           SMTLib2
import           SMTLib2.Int
import           Cmd.Solver  as Slv

import           Z3          (startZ3)
{-

X11 X21 X31 X41
X12 X22 X32 X42
X13 X23 X33 X43
X14 X24 X34 X44
-}

data Square = Square Int Int Int  

stringr Square a b c = "x" ++ show a ++ show b
 
sayfun:: String -> Expr
sayfun s = (App (I (N s) []) (Nothing) [])



declaratevar solver Square a b    =
	let a1 = "x" ++ show a ++ show b in   
	declareFun solver (N a1) [] tInt

{-
eclareVar :: (Int -> ..> IO Result)->Int -> Int -> IO Result
declareVar f 4 _ = +
declareVar f n k = declareVar f n+1 k

declareVar' :: Int -> Int -> Resutl  -> IO Result
declareVar n 4 = return x
declareVar n k = do
	declareFun "" n k
	declareVar' n k+1 
-}

create :: Int -> [[Int]]
create _ = do
	solver <- startZ3 Slv.Online "QF_LIA" Nothing Nothing
	declaratevar solver   Square 1 1 
	declaratevar solver   Square 1 2
	declaratevar solver   Square 1 3
 	declaratevar solver   Square 1 4
	declaratevar solver   Square 2 1 
	declaratevar solver   Square 2 2 
	declaratevar solver   Square 2 3 
	declaratevar solver   Square 2 4 
	declaratevar solver   Square 3 1 
	declaratevar solver   Square 3 2 
	declaratevar solver   Square 3 3 
	declaratevar solver   Square 3 4 
	declaratevar solver   Square 4 1 
	declaratevar solver   Square 4 2 
	declaratevar solver   Square 4 3 
	declaratevar solver   Square 4 4 

	assert solver (app (I (N "distinct") []) [(sayfun(stringr (Square 1 1))),(sayfun(stringr (Square 1 2)))])
	assert solver (app (I (N "distinct") []) [(sayfun(stringr (Square 1 1))),(sayfun(stringr (Square 1 3)))])
	assert solver (app (I (N "distinct") []) [(sayfun(stringr (Square 1 1))),(sayfun(stringr (Square 1 4)))])

	assert solver (app (I (N "distinct") []) [(sayfun(stringr (Square 1 2))),(sayfun(stringr (Square 1 3)))])
	assert solver (app (I (N "distinct") []) [(sayfun(stringr (Square 1 2))),(sayfun(stringr (Square 1 4)))])

	assert solver (app (I (N "distinct") []) [(sayfun(stringr (Square 1 3))),(sayfun(stringr (Square 1 4)))])

	assert solver (app (I (N "distinct") []) [(sayfun(stringr (Square 2 1))),(sayfun(stringr (Square 2 2)))])
	assert solver (app (I (N "distinct") []) [(sayfun(stringr (Square 2 1))),(sayfun(stringr (Square 2 3)))])
	assert solver (app (I (N "distinct") []) [(sayfun(stringr (Square 2 1))),(sayfun(stringr (Square 2 4)))])

	assert solver (app (I (N "distinct") []) [(sayfun(stringr (Square 2 2))),(sayfun(stringr (Square 2 3)))])
	assert solver (app (I (N "distinct") []) [(sayfun(stringr (Square 2 2))),(sayfun(stringr (Square 2 4)))])

	assert solver (app (I (N "distinct") []) [(sayfun(stringr (Square 2 3))),(sayfun(stringr (Square 2 4)))])

	assert solver (app (I (N "distinct") []) [(sayfun(stringr (Square 3 1))),(sayfun(stringr (Square 3 2)))])
	assert solver (app (I (N "distinct") []) [(sayfun(stringr (Square 3 1))),(sayfun(stringr (Square 3 3)))])
	assert solver (app (I (N "distinct") []) [(sayfun(stringr (Square 3 1))),(sayfun(stringr (Square 3 4)))])

	assert solver (app (I (N "distinct") []) [(sayfun(stringr (Square 3 2))),(sayfun(stringr (Square 3 3)))])
	assert solver (app (I (N "distinct") []) [(sayfun(stringr (Square 3 2))),(sayfun(stringr (Square 3 4)))])

	assert solver (app (I (N "distinct") []) [(sayfun(stringr (Square 3 3))),(sayfun(stringr (Square 3 4)))])

	assert solver (app (I (N "distinct") []) [(sayfun(stringr (Square 4 1))),(sayfun(stringr (Square 4 2)))])
	assert solver (app (I (N "distinct") []) [(sayfun(stringr (Square 4 1))),(sayfun(stringr (Square 4 3)))])
	assert solver (app (I (N "distinct") []) [(sayfun(stringr (Square 4 1))),(sayfun(stringr (Square 4 4)))])

	assert solver (app (I (N "distinct") []) [(sayfun(stringr (Square 4 2))),(sayfun(stringr (Square 4 3)))])
	assert solver (app (I (N "distinct") []) [(sayfun(stringr (Square 4 2))),(sayfun(stringr (Square 4 4)))])

	assert solver (app (I (N "distinct") []) [(sayfun(stringr (Square 4 3))),(sayfun(stringr (Square 4 4)))])	



	checkSat solver                        

 
 
	
 






