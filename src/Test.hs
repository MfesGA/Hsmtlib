import Test.HUnit

import           Hsmtlib
import           Hsmtlib.HighLevel
import           Hsmtlib.Solver    as Slv
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
    solver <- startSolver nSolver mode "AUFLIA" Nothing Nothing
    res <- produceModels solver
    exit solver
    assertEqual (err "t1") Success res

t2 :: Solvers -> Mode -> Assertion
t2 nSolver mode = do
    solver <- startSolver nSolver mode "AUFLIA" Nothing Nothing
    res <- produceModels solver
    exit solver
    assertEqual (err "t2") Success res

t3 :: Solvers -> Mode -> Assertion
t3 nSolver mode = do
    solver <- startSolver nSolver mode "AUFLIA" Nothing Nothing
    res <- declConst solver "x" tInt
    exit solver
    assertEqual (err "t3") Success res

t4 :: Solvers -> Mode -> Assertion
t4 nSolver mode = do
    solver <- startSolver nSolver mode "AUFLIA" Nothing Nothing
    res <- declConst solver "x" tBool
    exit solver
    assertEqual (err "t4") Success res




tc1 solver mode = TestCase $ t1 solver mode
tc2 solver mode = TestCase $ t2 solver mode
tc3 solver mode = TestCase $ t3 solver mode
tc4 solver mode = TestCase $ t4 solver mode




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



tests' solver mode = (tc1,solver, mode) 
                <@> tc2 
                <#> tc3
                <#=> tc4




main = runTestTT $ tests' Z3 Online

