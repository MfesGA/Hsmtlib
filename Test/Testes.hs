import           Hsmtlib
import           Hsmtlib.HighLevel      as H
import           Hsmtlib.Parsers.Syntax
import           Hsmtlib.Solver         as Slv
import           Prelude                hiding (not)
import           SMTLib2
import           SMTLib2.Array
import           SMTLib2.BitVector
import           SMTLib2.Core
import           SMTLib2.Int

main = 
    print "-----------Teste1--------" >> t1 >>
    print "-----------Teste2--------" >> t2 >>
    print "-----------Teste6--------" >> t6 >>
    print "-----------Teste7--------" >> t7 >>
    print "-----------Teste8--------" >> t8 >>
    print "-----------Teste9--------" >> t9 >>
    print "-----------Teste10--------" >> t10

t1 :: IO ()
t1 = do 
    solver <- startSolver Z3 Slv.Script AUFLIA Nothing (Just "teste1.smt2") 
    produceModels solver >>= print
    exit solver >>= print

t2 :: IO ()
t2 = do 
    solver <- startSolver Z3 Slv.Script AUFLIA Nothing (Just "teste1.smt2") 
    declConst solver "a" tInt >>= print 
    declFun solver "f" [tInt, tBool] tInt >>= print 
    Slv.assert solver (nLt  (H.ct "a") (lit 10)) >>= print 
    Slv.assert solver (nGt  (fun "f" [H.ct "a", true]) (lit 100) ) >>= print 
    checkSat solver >>= print 
    exit solver >>= print


t6 :: IO ()
t6 = do
     solver <- startSolver Z3 Slv.Script AUFLIA Nothing (Just "teste6.smt2")
     declConst solver "a" tInt >>= print
     declFun solver "f" [tInt, tBool] tInt >>= print
     Slv.assert solver (nLt  (H.ct "a") (lit 10)) >>= print
     Slv.assert solver (nGt  (fun "f" [H.ct "a", true]) (lit 100) ) >>= print
     checkSat solver >>= print
     exit solver >>= print



t7 :: IO ()
t7 = do
     solver <- startSolver Z3 Slv.Script AUFLIA Nothing (Just "teste7.smt2")
     declConst solver "x" tInt >>= print
     declConst solver "y" tInt >>= print
     declConst solver "z" tInt >>= print
     push solver 1 >>= print
     Slv.assert solver ( (ct "x" `nAdd` ct "y") === lit 10) >>= print
     Slv.assert solver ( ((  lit 3 `nMul` ct "x") `nAdd` ct "x ") === lit 20) >>= print
     checkSat solver >>= print
     pop solver 1 >>= print
     push solver 1 >>= print
     Slv.assert solver ((( ct "x" `nMul` lit 3)`nAdd` ct "y")=== lit 10) >>= print
     Slv.assert solver (( ( lit 2 `nMul` ct "x")`nAdd` ( lit 2 `nMul` ct "y")) === lit 21) >>= print
     checkSat solver >>= print
     declConst solver "p" tBool >>= print
     pop solver 1 >>= print
     Slv.assert solver (ct "p") >>= print
     exit solver >>= print




t8 :: IO ()
t8 = do
     solver <- startSolver Z3 Slv.Script QF_ABV Nothing (Just "teste8.smt2")
     declConst solver "x" (tBitVec 64) >>= print
     declConst solver "y" (tBitVec 64) >>= print
     Slv.assert solver (not ((bvnot (ct "x") `bvand` bvnot (ct "y")) === bvnot (bvor (ct "x") (ct "y")))) >>= print
     checkSat solver >>= print
     exit solver >>= print

t9 :: IO ()
t9 = do
     solver <- startSolver Z3 Slv.Script QF_ABV Nothing (Just "teste9.smt2")
     produceModels solver >>= print
     declConst solver "x" (tBitVec 4) >>= print
     declConst solver "y" (tBitVec 4) >>= print
     Slv.assert solver (not (bvule (ct "x") (ct "y")) === bvsle (ct "x") (ct "y")) >>= print
     checkSat solver >>= print
     getValue solver [ct "x", ct "y"] >>= print
     exit solver >>= print


t10 :: IO ()
t10 = do
     solver <- startSolver Z3 Slv.Script AUFLIA Nothing (Just "teste10.smt2")
     produceModels solver >>= print
     declConst solver "x" tInt >>= print
     declConst solver "y" tInt >>= print
     declConst solver "z" tInt >>= print
     declConst solver "a1" ( tArray tInt tInt) >>= print
     declConst solver "a2" ( tArray tInt tInt) >>= print
     declConst solver "a3" ( tArray tInt tInt) >>= print
     Slv.assert solver (select (ct "a1") (ct "x") === ct "x") >>= print
     Slv.assert solver (store  (ct "a1") (ct "x") (ct"y")  === ct "a1") >>= print
     checkSat solver >>= print
     getValue solver [ct "x", ct "y", ct"z", select (ct "a1") (ct "x"),select (ct "a2") (ct "y")] >>= print
     getValue solver [ct "x", ct "y", ct"z", select (ct "a1") (ct "x"),select (ct "a2") (ct "y")] >>= print

     exit solver >>= print
