{- |
  Module      : Hsmtlib
  Main module which provides the function to initialize a Solver.
-}

module Hsmtlib(startSolver) where

import           Hsmtlib.Solver            as Slv
import           Hsmtlib.Solvers.Altergo   (startAltErgo)
import           Hsmtlib.Solvers.Boolector (startBoolector)
import           Hsmtlib.Solvers.Cvc4      (startCvc4)
import           Hsmtlib.Solvers.MathSAT   (startMathSat)
import           Hsmtlib.Solvers.Yices     (startYices)
import           Hsmtlib.Solvers.Z3        (startZ3)
{-import           Hsmtlib.HighLevel
import           Hsmtlib.Parsers.AuxParser
import           Hsmtlib.Parsers.ParseResponse
import           Hsmtlib.Parsers.ParseScript
import           Hsmtlib.Parsers.Syntax
import           Hsmtlib.Parsers.Visualizer-}

{- |  

The function to initialialyze a solver.
The solver can be initialized with a desired configuration, or a diferent
Path to keep the script in Script Mode, if Nothing is passed then it will use
the default settings.

There are two 'Mode's of operation for the solvers, Online and Script.

In online Mode a solver is created and kept running. Commands are sent
via pipe one by one and every time one is sent it also reads the answer of the
solver.

In script 'Mode' a file is created in a desired file path, if Nothing is passed
then its created in the current directory with the name temp.smt2.
If a file already exists then it's overwriten.

The functions in this mode (Script) behave in the following manner:
If it's a funcion where something is declared, for example declareFun or assert
then it's only writen to the file. In functions where some feedback is expected
such as checkSat, this are writen to the file, a solver is created and the
file is given to solver, and it waits for the result. The result is the result
of the last function.

-}



startSolver :: Solvers -- ^ Avaliable'Solvers'.
            -> Mode -- ^ Avaliable 'Modes', Online, Script, Context.
            -> Logic -- ^ The desired SMT Logic.
            -> Maybe SolverConfig -- ^ A customized Configuration for the Solver.
            -> Maybe String -- ^  A possible alternate path to save the Script.
            -> IO Solver
startSolver Z3 mode logic = startZ3 mode $ show logic  
startSolver Cvc4 mode logic = startCvc4 mode $ show logic
startSolver Yices mode logic = startYices mode $ show logic
startSolver Mathsat mode logic = startMathSat mode $ show logic
startSolver Altergo mode logic = startAltErgo mode $ show logic
startSolver Boolector mode logic = startBoolector mode $ show logic
