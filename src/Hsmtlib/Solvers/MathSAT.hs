{- |
Module      : Hsmtlib.Solvers.MathSAT
  Module wich has the standard configuration for all mathSat Modes and
  provides the initilizing function.
-}
module Hsmtlib.Solvers.MathSAT(startMathSat) where

import           Hsmtlib.Solver                      as Slv
import           Hsmtlib.Solvers.Cmd.OnlineCmd
import           Hsmtlib.Solvers.Cmd.ProcCom.Process
import           Hsmtlib.Solvers.Cmd.ScriptCmd
import           Smtlib.Syntax.Syntax
import           System.IO                           (Handle,
                                                      IOMode (WriteMode),
                                                      openFile)

-- All the configurations are the same but have diferent names so if anything
-- changes it's easy to alter its configuration.


mathSatConfig :: SolverConfig
mathSatConfig = Config { path = "mathsat"
                       , version = "5"
                       }

stdFlags = []


startMathSat :: Maybe SolverConfig -> IO Solver
startMathSat Nothing = startMathSat' mathSatConfig
startMathSat (Just cfg) = startMathSat' cfg



startMathSat' :: SolverConfig -> IO Solver
startMathSat' cfg = do
  -- Starts a Z4 Process.
  process <- beginProcess (path cfg) stdFlags
  --Set Option to print success after accepting a Command.
  _ <- onlineSetOption Mathsat process (PrintSuccess True)
  -- Initialize the solver Functions and return them.
  return $ onlineSolver process

--Start mathSat Script.



-- Creates the functions for online mode with the process already running.
-- Each function will send the command to the solver and wait for the response.
onlineSolver :: Process -> Solver
onlineSolver process =
  Solver { setLogic = onlineSetLogic Mathsat process
         , setOption = onlineSetOption Mathsat process
         , setInfo = onlineSetInfo Mathsat process
         , declareSort = onlineDeclareSort Mathsat process
         , defineSort = onlineDefineSort Mathsat process
         , declareFun = onlineDeclareFun Mathsat process
         , defineFun = onlineDefineFun Mathsat process
         , push = onlinePush Mathsat process
         , pop = onlinePop Mathsat process
         , assert = onlineAssert Mathsat process
         , checkSat = onlineCheckSat Mathsat process
         , getAssertions = onlineGetAssertions Mathsat process
         , getValue = onlineGetValue Mathsat process
         , getProof = onlineGetProof Mathsat process
         , getUnsatCore = onlineGetUnsatCore Mathsat process
         , getInfo = onlineGetInfo Mathsat process
         , getOption = onlineGetOption Mathsat process
         , exit = onlineExit process
         }

