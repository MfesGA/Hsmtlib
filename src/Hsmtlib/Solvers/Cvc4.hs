{- |
Module      : Hsmtlib.Solvers.Cvc4
  Module wich has the standard configuration for all Cvc4 Modes and
  provides the initilizing function.
-}
module Hsmtlib.Solvers.Cvc4(startCvc4) where


import           Hsmtlib.Solver                      as Slv
import           Hsmtlib.Solvers.Cmd.OnlineCmd
import           Hsmtlib.Solvers.Cmd.ProcCom.Process
import           Hsmtlib.Solvers.Cmd.ScriptCmd
import           Smtlib.Syntax.Syntax
import           System.IO                           (Handle,
                                                      IOMode (WriteMode),
                                                      openFile)

{-
    TODO: Why the flag --status frezzes the process.
-}

cvc4ConfigOnline :: SolverConfig
cvc4ConfigOnline =
    Config { path = "cvc4"
           , args = ["--interactive", "--smtlib-strict", "--print-success", "-q"]
           }



startCvc4:: IO Solver
startCvc4 = do
  -- Starts a Cvc4 Process.
  process <- beginProcess (path cvc4ConfigOnline) (args cvc4ConfigOnline)
  -- Set Option to print success after accepting a Command.
  _ <- onlineSetOption Mathsat process (PrintSuccess True)
  -- Initialize the solver Functions and return them.
  return $ onlineSolver process




-- Creates the functions for online mode with the process already running.
-- Each function will send the command to the solver and wait for the response.
onlineSolver :: Process -> Solver
onlineSolver process =
  Solver { setLogic = onlineSetLogic Cvc4 process
         , setOption = onlineSetOption Cvc4 process
         , setInfo = onlineSetInfo Cvc4 process
         , declareSort = onlineDeclareSort Cvc4 process
         , defineSort = onlineDefineSort Cvc4 process
         , declareFun = onlineDeclareFun Cvc4 process
         , defineFun = onlineDefineFun Cvc4 process
         , push = onlinePush Cvc4 process
         , pop = onlinePop Cvc4 process
         , assert = onlineAssert Cvc4 process
         , checkSat = onlineCheckSat Cvc4 process
         , getAssertions = onlineGetAssertions Cvc4 process
         , getValue = onlineGetValue Cvc4 process
         , getProof = onlineGetProof Cvc4 process
         , getUnsatCore = onlineGetUnsatCore Cvc4 process
         , getInfo = onlineGetInfo Cvc4 process
         , getOption = onlineGetOption Cvc4 process
         , exit = onlineExit process
         }

