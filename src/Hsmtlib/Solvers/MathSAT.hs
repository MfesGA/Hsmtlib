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


mathSatConfigOnline :: SolverConfig
mathSatConfigOnline =
        Config { path = "mathsat"
               , args = []
               }

mathSatConfigScript :: SolverConfig
mathSatConfigScript =
        Config { path = "mathsat"
               , args = []
               }

mathSatConfigBatch :: SolverConfig
mathSatConfigBatch =
        Config { path = "mathsat"
               , args = []
               }

{- |
  Function that initialyzes a mathSat Solver.
  It Receives a Mode, an SMT Logic, it can receive a diferent configuration
  for the solver and an anternative path to create the script in Script Mode.

  In Online Mode if a FilePath is passed then it's ignored.
-}
startMathSat :: Mode
             -> String
             -> Maybe SolverConfig
             -> Maybe FilePath
             -> IO Solver

startMathSat Slv.Online logic sConf _ = startMathSatOnline logic sConf
startMathSat Slv.Script logic sConf scriptFilePath =
    startMathSatScript logic sConf scriptFilePath

-- Start mathSat Online.

startMathSatOnline :: String -> Maybe SolverConfig -> IO Solver
startMathSatOnline logic Nothing =
  startMathSatOnline' logic mathSatConfigOnline

startMathSatOnline logic (Just conf) = startMathSatOnline' logic conf

startMathSatOnline' :: String -> SolverConfig -> IO Solver
startMathSatOnline' logic conf = do
  -- Starts a Z4 Process.
  process <- beginProcess (path conf) (args conf)
  --Set Option to print success after accepting a Command.
  _ <- onlineSetOption Mathsat process (PrintSuccess True)
  -- Sets the SMT Logic.
  _ <- onlineSetLogic Mathsat process logic
  -- Initialize the solver Functions and return them.
  return $ onlineSolver process

--Start mathSat Script.

startMathSatScript :: String -> Maybe SolverConfig -> Maybe FilePath -> IO Solver
startMathSatScript logic Nothing Nothing =
    startMathSatScript' logic mathSatConfigScript "temp.smt2"
startMathSatScript logic (Just conf) Nothing =
    startMathSatScript' logic conf "temp.smt2"
startMathSatScript logic Nothing (Just scriptFilePath) =
    startMathSatScript' logic mathSatConfigScript scriptFilePath
startMathSatScript logic (Just conf) (Just scriptFilePath) =
    startMathSatScript' logic conf scriptFilePath

{-
  In this function a file is created where the commands are kept.

  Every function in the ScriptCmd Module needs a ScriptConf data which has:

  - sHandle: The handle of the script file
  - sCmdPath: The Path to initilyze the solver
  - sArgs: The options of the solver
  - sFilePath: The file path of the script so it can be passed to the solver
               when started.
-}
startMathSatScript' :: String -> SolverConfig -> FilePath -> IO Solver
startMathSatScript' logic conf scriptFilePath = do
  scriptHandle <- openFile scriptFilePath WriteMode
  let srcmd = newScriptArgs conf scriptHandle scriptFilePath
  _ <- scriptSetOption srcmd (PrintSuccess True)
  _ <- scriptSetLogic srcmd logic
  return $ scriptSolver srcmd

--Function which creates the ScriptConf for the script functions.
newScriptArgs :: SolverConfig  -> Handle -> FilePath -> ScriptConf
newScriptArgs solverConfig nHandle scriptFilePath =
  ScriptConf { sHandle = nHandle
             , sCmdPath = path solverConfig
             , sArgs = args solverConfig
             , sFilePath  = scriptFilePath
             }



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

-- Creates the funtion for the script mode.
-- The configuration of the file is passed.
scriptSolver :: ScriptConf -> Solver
scriptSolver srcmd =
  Solver { setLogic = scriptSetLogic srcmd
         , setOption = scriptSetOption srcmd
         , setInfo = scriptSetInfo srcmd
         , declareSort = scriptDeclareSort srcmd
         , defineSort = scriptDefineSort srcmd
         , declareFun = scriptDeclareFun srcmd
         , defineFun = scriptDefineFun srcmd
         , push = scriptPush srcmd
         , pop = scriptPop srcmd
         , assert = scriptAssert srcmd
         , checkSat = scriptCheckSat srcmd
         , getAssertions = scriptGetAssertions srcmd
         , getValue = scriptGetValue srcmd
         , getProof = scriptGetProof srcmd
         , getUnsatCore = scriptGetUnsatCore srcmd
         , getInfo = scriptGetInfo srcmd
         , getOption = scriptGetOption srcmd
         , exit = scriptExit srcmd
         }
