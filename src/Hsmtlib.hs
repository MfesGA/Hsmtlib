{- |
Module      : Hsmtlib
  Main module which provides the function to initialyze a Solver.
-}

module Hsmtlib(startSolver) where

import           Hsmtlib.Solver            as Slv
import           Hsmtlib.Solvers.Altergo   (startAltErgo)
import           Hsmtlib.Solvers.Boolector (startBoolector)
import           Hsmtlib.Solvers.Cvc4      (startCvc4)
import           Hsmtlib.Solvers.MathSAT   (startMathSat)
import           Hsmtlib.Solvers.Yices     (startYices)
import           Hsmtlib.Solvers.Z3        (startZ3)
import           SMTLib2
import           SMTLib2.Int



{- |  The function to initialialyze a solver.
The solver can be initialized with a desired configuration, or a diferent
Path to keep the script in Script Mode, if Nothing is passed then it will use
the default settings.

There exists three 'Mode's that a solver can be used, Online,
Context and Script.

In online Mode a solver is created and kept running. Commands are sent
via pipe one by one and every time one is sent it also reads the answer of the
solver.

In script 'Mode' a file is created in a desired file path, if Nothing is passed
then its created in the current directory with the name temp.smt2.
If a file already exists then it's overwriten.

The functions in this mode behave in the following manner:
If it's a funcion where something is declared, for example declareFun or assert
then it's only writen to the file. In functions where some feedback is expected
such as checkSat, this are writen to the file, a solver is created and the
file is given to solver, and it waits for the result. The result is the result
of the last function.

In context 'Mode' two lists are kept, a list with all the commands given and
a list that has the results of commands that demand some output from the solver.

The functions in this mode behave has in script 'Mode' being the diference that
instead of a file the commands are kept in the list.

In order to use the context mode it's nedded some special operators that
aren't nedded in other modes.

The operators and there uses can be found in 'Solver'




* Example of online or script mode :

>main :: IO ()
>main = do
  >solver <- startSolver Z3 Online "QF_LIA"  Nothing Nothing
  >declareFun solver (N "a") [] tInt >>= print
  >declareFun solver (N "x") [] tInt >>= print
  >declareFun solver (N "y") [] tInt >>= print
  >declareFun solver (N "f") [] tInt >>= print
  >checkSat solver >>= print
  >exit solver >>= print


* Example of context mode:

>main :: IO ()
>main = do
  >solver <- startSolver Cvc4 Slv.Context "QF_LIA"  Nothing Nothing
  >declareFunCt solver (N "a") [] tInt |*|
  >declareFunCt solver (N "x") [] tInt |#|
  >declareFunCt solver (N "y") [] tInt |#|
  >checkSatCt solver |$|
  >exitCt solver >>= print


-}



startSolver :: Solvers-- ^ Avaliable'Solvers'.
            -> Mode-- ^ Avaliable 'Modes', Online, Script, Context.
            -> String-- ^ The desired SMT Logic.
            -> Maybe SolverConfig-- ^ A customized Configuration for the Solver.
            -> Maybe FilePath-- ^  A possible alternate path to save the Script.
            -> IO Solver
startSolver Z3 = startZ3
startSolver Cvc4 = startCvc4
startSolver Yices = startYices
startSolver Mathsat= startMathSat
startSolver Altergo= startAltErgo
startSolver Boolector= startBoolector


main :: IO ()
main = do
  solver <- startSolver Z3 Online "QF_LIA"  Nothing Nothing
  setLogic solver (N "QF_LIA") >>= print
  declareFun solver (N "a") [] tInt >>= print
  declareFun solver (N "x") [] tInt >>= print
  declareFun solver (N "y") [] tInt >>= print
  declareFun solver (N "f") [] tInt >>= print
  getAssertions solver >>= print
  checkSat solver >>= print
  exit solver >>= print