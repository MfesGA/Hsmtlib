{- |
Module      : Hsmtlib.Solvers.Z3
  Module wich has the standard configuration for all Z3 Modes and
  provides the initilizing function.
-}
module Hsmtlib.Solvers.Z3(startZ3) where

import           Control.Applicative                  (liftA)
import           Hsmtlib.Solver                      as Slv
import           Hsmtlib.Solvers.Cmd.OnlineCmd
import           Hsmtlib.Solvers.Cmd.ProcCom.Process
import           Hsmtlib.Solvers.Cmd.ScriptCmd
import           Hsmtlib.Solvers.Cmd.CmdResult
import           Smtlib.Syntax.Syntax
import           System.IO                           (Handle,
                                                      IOMode (WriteMode),
                                                      openFile)

-- All the configurations are the same but have diferent names so if anything
-- changes it's easy to alter its configuration.

z3Config :: SolverConfig
z3Config = Config { path = "z3"
                  , version = "4.3"
                  }

stdFlags = ["-smt2","-in"]

{- |
  Function that initialyzes a Z3 Solver.
  It Receives a Mode, an SMT Logic, it can receive a diferent configuration
  for the solver and an anternative path to create the script in Script Mode.

  In Online Mode if a FilePath is passed then it's ignored.
-}

 
startZ3 :: Maybe SolverConfig -> IO Solver
startZ3 Nothing = startZ3' z3Config
startZ3 (Just cfg) = startZ3' cfg

startZ3' :: SolverConfig -> IO Solver
startZ3' config = do
  -- Starts a Z3 Process.
  process <- beginProcess (path config) stdFlags
  --Set Option to print success after accepting a Command.
  onlineSetOption Z3 process (PrintSuccess True)
  -- Initialize the solver Functions and return them.
  return $ onlineSolver process



-- Creates the functions for online mode with the process already running.
-- Each function will send the command to the solver and wait for the response.
onlineSolver :: Process -> Solver
onlineSolver process =
  Solver { setLogic = onlineSetLogic Z3 process
         , setOption = onlineSetOption Z3  process
         , setInfo = onlineSetInfo Z3  process
         , declareSort = onlineDeclareSort Z3  process
         , defineSort = onlineDefineSort Z3  process
         , declareFun = onlineDeclareFun Z3  process
         , defineFun = onlineDefineFun Z3  process
         , push = onlinePush Z3 process 
         , pop = onlinePop Z3 process
         , assert = onlineAssert Z3 process
         , checkSat = onlineCheckSat Z3 process
         , getAssertions = onlineGetAssertions Z3 process
         , getValue = onlineGetValue Z3 process
         , getProof = onlineGetProof Z3 process
         , getUnsatCore = onlineGetUnsatCore Z3 process
         , getInfo = onlineGetInfo Z3 process
         , getOption = onlineGetOption Z3 process
         , exit = onlineExit process
         }

