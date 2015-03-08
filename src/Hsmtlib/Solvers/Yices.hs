{- |
Module      : Hsmtlib.Solvers.Yices
  Module wich has the standard configuration for all Yices Modes and
  provides the initilizing function.
-}
module Hsmtlib.Solvers.Yices(startYices) where

import           Hsmtlib.Solver                      as Slv
import           Hsmtlib.Solvers.Cmd.OnlineCmd
import           Hsmtlib.Solvers.Cmd.ProcCom.Process
import           Hsmtlib.Solvers.Cmd.ScriptCmd
import           Smtlib.Syntax.Syntax
import           System.IO                           (Handle,
                                                      IOMode (WriteMode),
                                                      openFile)


yicesConfigOnline :: SolverConfig
yicesConfigOnline =
    Config { path = "yices-smt2"
           , args = [ "--interactive"]
           }






startYices:: IO Solver
startYices = do
  -- Starts a Yices Process.
  process <- beginProcess (path yicesConfigOnline) (args yicesConfigOnline)
  --Set Option to print success after accepting a Command.
  _ <- onlineSetOption Yices process (PrintSuccess True)
  -- Initialize the solver Functions and return them.
  return $ onlineSolver process





-- Creates the functions for online mode with the process already running.
-- Each function will send the command to the solver and wait for the response.
onlineSolver :: Process -> Solver
onlineSolver process =
  Solver { setLogic = onlineSetLogic Yices process
         , setOption = onlineSetOption Yices process
         , setInfo = onlineSetInfo Yices process
         , declareSort = onlineDeclareSort Yices process
         , defineSort = onlineDefineSort Yices process
         , declareFun = onlineDeclareFun Yices process
         , defineFun = onlineDefineFun Yices process
         , push = onlinePush Yices process
         , pop = onlinePop Yices process
         , assert = onlineAssert Yices process
         , checkSat = onlineCheckSat Yices process
         , getAssertions = onlineGetAssertions Yices process
         , getValue = onlineGetValue Yices process
         , getProof = onlineGetProof Yices process
         , getUnsatCore = onlineGetUnsatCore Yices process
         , getInfo = onlineGetInfo Yices process
         , getOption = onlineGetOption Yices process
         , exit = onlineExit process
         }

