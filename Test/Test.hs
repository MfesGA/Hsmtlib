module Test where


import           Test.HUnit

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


err :: String -> String
err teste =
 "-------------------------------" ++ teste ++ "------------------------------"

t1 :: Solvers -> Mode -> Assertion
t1 nSolver  mode = do
    solver <- startSolver nSolver mode AUFLIA Nothing (Just "teste1.smt2")
    res <- produceModels solver
    exit solver
    assertEqual (err "t1") (CGR Success) res

t2 :: Solvers -> Mode -> Assertion
t2 nSolver mode = do
    solver <- startSolver nSolver mode AUFLIA Nothing (Just "teste2.smt2")
    res <- produceModels solver
    exit solver
    assertEqual (err "t2") (CGR Success) res

t3 :: Solvers -> Mode -> Assertion
t3 nSolver mode = do
    solver <- startSolver nSolver mode AUFLIA Nothing (Just "teste3.smt2")
    res <- declConst solver "x" tInt
    exit solver
    assertEqual (err "t3") (CGR Success) res

t4 :: Solvers -> Mode -> Assertion
t4 nSolver mode = do
    solver <- startSolver nSolver mode AUFLIA Nothing (Just "teste4.smt2")
    res <- declConst solver "x" tBool
    exit solver
    assertEqual (err "t4") (CGR Success) res


t5 :: Solvers -> Mode -> Assertion
t5 nSolver mode = do
    solver <- startSolver nSolver mode AUFLIA Nothing (Just "teste5.smt2")
    res <- declareFun solver (N "System.Reflection.ICustomAttributeProvider") [ ] tInt
    exit solver
    assertEqual (err "t6") (CGR Success) res

t6 :: Solvers -> Mode -> Assertion
t6 nSolver mode = do
    solver <- startSolver nSolver mode AUFLIA Nothing (Just "teste6.smt2")
    decA <- declConst solver "a" tInt
    decFun <- declFun solver "f" [tInt, tBool] tInt
    assertLt <- Slv.assert solver (nLt  (H.ct "a") (lit 10))
    assertGt <- Slv.assert solver (nGt  (fun "f" [H.ct "a", true]) (lit 100) )
    res <-checkSat solver
    exit solver
    assertEqual (err "t6 decA")  (CGR Success) decA
    assertEqual (err "t6 decFun") (CGR Success) decFun
    assertEqual (err "t6 assertLt") (CGR Success) assertLt
    assertEqual (err "t6 assertGt") (CGR Success) assertGt
    assertEqual (err "t6") (CCS Sat) res


t7 :: Solvers -> Mode -> Assertion
t7 nSolver mode = do
    solver <- startSolver nSolver mode AUFLIA Nothing (Just "teste7.smt2")
    decA <- declConst solver "x" tInt
    decY <- declConst solver "y" tInt
    decZ <- declConst solver "z" tInt
    pu <- push solver 1
    assert1 <- Slv.assert solver ( (ct "x" `nAdd` ct "y") === lit 10)
    assert2 <- Slv.assert solver ( ((  lit 3 `nMul` ct "x") `nAdd` ct "x ") === lit 20)
    sat <- checkSat solver
    po <- pop solver 1
    pu2 <- push solver 1
    assert3 <- Slv.assert solver ((( ct "x" `nMul` lit 3)`nAdd` ct "y")=== lit 10)
    assert4 <- Slv.assert solver (( ( lit 2 `nMul` ct "x")`nAdd` ( lit 2 `nMul` ct "y")) === lit 21)
    sat2 <- checkSat solver
    decP <- declConst solver "p" tBool
    po2 <- pop solver 1
    assert5 <- Slv.assert solver $ ct "p"
    exit solver
    assertEqual (err "t7 decA")  (CGR Success) decA
    assertEqual (err "t7 decY")  (CGR Success) decY
    assertEqual (err "t7 decZ")  (CGR Success) decZ
    assertEqual (err "t7 pu")  (CGR Success) pu
    assertEqual (err "t7 assert1")  (CGR Success) assert1
    assertEqual (err "t7 assert2")  (CGR Success) assert2
    assertEqual (err "t7 sat") (CCS Sat) sat
    assertEqual (err "t7 po")  (CGR Success) po
    assertEqual (err "t7 pu2")  (CGR Success) pu2
    assertEqual (err "t7 assert3")  (CGR Success) assert3
    assertEqual (err "t7 assert4")  (CGR Success) assert4
    assertEqual (err "t7 sat2") (CCS Unsat) sat2
    assertEqual (err "t7 decP")  (CGR Success) decP
    assertEqual (err "t7 po2")  (CGR Success) po2
    assertEqual (err "t7 assert5")  (CGR Success) assert5



t8 :: Solvers -> Mode -> Assertion
t8 nSolver mode = do
    solver <- startSolver nSolver mode QF_ABV Nothing (Just "teste8.smt2")
    decX <- declConst solver "x" $ tBitVec 64
    decY <- declConst solver "y" $ tBitVec 64
    assert <- Slv.assert solver $ not ((bvnot (ct "x") `bvand` bvnot (ct "y")) === bvnot (bvor (ct "x") (ct "y")))
    sat <- checkSat solver
    assertEqual (err "t8 decX") (CGR Success) decX
    assertEqual (err "t8 decY") (CGR Success) decY
    assertEqual (err "t8 assert") (CGR Success) assert
    assertEqual (err "t8 sat") (CCS Unsat) sat

t9 :: Solvers -> Mode -> Assertion
t9 nSolver mode = do
    solver <- startSolver nSolver mode QF_ABV Nothing (Just "teste9.smt2")
    produceModels solver
    decX <- declConst solver "x" $ tBitVec 4
    decY <- declConst solver "y" $ tBitVec 4
    assert <- Slv.assert solver $ not (bvule (ct "x") (ct "y")) === bvsle (ct "x") (ct "y")
    sat <- checkSat solver
    vals <- getValue solver [ct "x", ct "y"]
    putStr "\n"
    print $ "Vals t9: " ++ show vals
    assertEqual (err "t9 decX") (CGR Success) decX
    assertEqual (err "t9 decY") (CGR Success) decY
    assertEqual (err "t9 assert") (CGR Success) assert
    assertEqual (err "t9 sat") (CCS Sat) sat


t10 :: Solvers -> Mode -> Assertion
t10 nSolver mode = do
    solver <- startSolver nSolver mode AUFLIA Nothing (Just "teste10.smt2")
    produceModels solver
    decX <- declConst solver "x" tInt
    decY <- declConst solver "y" tInt
    decZ <- declConst solver "z" tInt
    decA1 <- declConst solver "a1" $ tArray tInt tInt
    decA2 <- declConst solver "a2" $ tArray tInt tInt
    decA3 <- declConst solver "a3" $ tArray tInt tInt
    assert1 <- Slv.assert solver $ select (ct "a1") (ct "x") === ct "x"
    assert2 <- Slv.assert solver $ store  (ct "a1") (ct "x") (ct"y")  === ct "a1"
    sat <- checkSat solver
    vals <- getValue solver [ct "x", ct "y", ct"z", select (ct "a1") (ct "x"),select (ct "a2") (ct "y")]
    putStr "\n"
    print $ "vals t10: " ++ show vals   
    assertEqual (err "t10 decX") (CGR Success) decX
    assertEqual (err "t10 decY") (CGR Success) decY
    assertEqual (err "t10 decZ") (CGR Success) decZ
    assertEqual (err "t10 decA1") (CGR Success) decA1
    assertEqual (err "t10 decA2") (CGR Success) decA2
    assertEqual (err "t10 decA3") (CGR Success) decA3
    assertEqual (err "t10 assert1") (CGR Success) assert1
    assertEqual (err "t10 assert2") (CGR Success) assert2
    assertEqual (err "t10 sat") (CCS Sat) sat


tc1 solver mode = TestCase $ t1 solver mode
tc2 solver mode = TestCase $ t2 solver mode
tc3 solver mode = TestCase $ t3 solver mode
tc4 solver mode = TestCase $ t4 solver mode
tc5 solver mode = TestCase $ t5 solver mode
tc6 solver mode = TestCase $ t6 solver mode
tc7 solver mode = TestCase $ t7 solver mode
tc8 solver mode = TestCase $ t8 solver mode
tc9 solver mode = TestCase $ t9 solver mode
tc10 solver mode = TestCase $ t10 solver mode




(<@>) :: (Solvers -> Mode -> Test, Solvers, Mode)
      -> (Solvers -> Mode -> Test)
      -> ([Test],Solvers,Mode)

(<@> ) (f, s, m) g = ( g s m : [f s m], s, m)

(<#>) :: ([Test],Solvers,Mode)
      -> (Solvers -> Mode -> Test)
      -> ([Test], Solvers, Mode)
(<#>)  (res, s, m) f = (f s m:res,s,m)

(<#=>) :: ([Test],Solvers,Mode)
      -> (Solvers -> Mode -> Test)
      -> Test
(<#=>) (res, s, m) f = TestList $ f s m : res


tests' :: Solvers -> Mode -> Test
tests' solver mode = (tc1,solver, mode)
                <@> tc2
                <#> tc3
                <#> tc4
                <#> tc5
                <#> tc6
                <#> tc7
                <#> tc8
                <#> tc9
                <#=> tc10

main :: IO Counts
main = runTestTT $ tests' Z3 Slv.Script  
