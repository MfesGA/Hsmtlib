{- |
  Module      : Hsmtlib
  Main module which provides the function to initialize a Solver.
-}

module Hsmtlib(startSolver) where

import           Hsmtlib.Solver            as Slv

import Smtlib.Parsers.CommandsParsers
import Smtlib.Syntax.Syntax
import Text.ParserCombinators.Parsec
import Data.Functor
import Control.Monad


import           Hsmtlib.Solvers.Z3        (startZ3)
import           Hsmtlib.Solvers.MathSAT   (startMathSat)
import           Hsmtlib.Solvers.Cvc4      (startCvc4)


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
			-> Maybe SolverConfig 
            -> IO Solver
startSolver Z3 = startZ3
startSolver Mathsat = startMathSat
startSolver Cvc4 = startCvc4


executeScript :: Solvers -> Maybe SolverConfig -> String -> IO [Result]
executeScript solver cfg path  = parseFile path >>= parseRes solver cfg

parseRes :: Solvers -> Maybe SolverConfig -> (Either ParseError Source) -> IO [Result]
parseRes _ _ (Left error) = return [ComError (show error)]
parseRes solver cfg (Right cmds) =executeCommands solver cfg cmds


executeCommands :: Solvers -> Maybe SolverConfig -> Source -> IO [Result]
executeCommands solver cfg cmds = do 
	solv <- startSolver solver cfg
	mapM (execCmd solv) cmds


execCmd :: Solver -> Command  -> IO Result
execCmd solver (SetLogic logic) = setLogic solver logic
execCmd solver (SetOption opt) = setOption solver opt
execCmd solver (SetInfo attr) = setInfo solver attr
execCmd solver (DeclareSort symb numb) = declareSort solver symb numb
execCmd solver (DefineSort symb symbs sort) = defineSort solver symb symbs  sort
execCmd solver (DeclareFun str sorts sort) = declareFun solver str sorts sort
execCmd solver (DefineFun str vars sort term) = 
	defineFun solver str vars sort term
execCmd solver (Push n) = push solver n
execCmd solver (Pop n) = pop solver n
execCmd solver (Assert term) = assert solver term
execCmd solver CheckSat = checkSat solver
execCmd solver GetAssertions = getAssertions solver
execCmd solver GetProof = getProof solver
execCmd solver GetUnsatCore = getUnsatCore solver
execCmd solver (GetValue terms) = getValue solver terms
--execCmd GetAssignment solver = getAssignment solver
execCmd solver (GetOption opt) = getOption solver opt
execCmd solver (GetInfo info) = getInfo solver info
execCmd solver Exit = exit solver


parseFile :: FilePath -> IO (Either ParseError Source)
parseFile x = parse parseSource "" <$> readFile x