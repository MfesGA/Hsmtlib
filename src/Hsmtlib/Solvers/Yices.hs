{- |
Module      : Hsmtlib.Solvers.Yices
  Module wich has the standard configuration for all Yices Modes and
  provides the initilizing function.
-}
module Hsmtlib.Solvers.Yices(startYices) where

import           Hsmtlib.Solver                      as Slv
import           Hsmtlib.Solvers.Cmd.BatchCmd        as B (executeBatch)
import           Hsmtlib.Solvers.Cmd.OnlineCmd
import           Hsmtlib.Solvers.Cmd.ProcCom.Process
import           Hsmtlib.Solvers.Cmd.ScriptCmd
import           SMTLib2
import           System.IO                           (Handle,
                                                      IOMode (WriteMode),
                                                      openFile)


yicesConfigOnline :: SolverConfig
yicesConfigOnline =
    Config { path = "yices-smt2"
           , args = [ "--interactive"]
           }

-- Script configurations is the same but has diferent names
-- so if anything  changes it's easy to alter its configuration.

yicesConfigScript :: SolverConfig
yicesConfigScript =
    Config { path = "yices-smt2"
           , args = []
           }

yicesConfigBatch :: SolverConfig
yicesConfigBatch =
        Config { path = "yices-smt2"
               , args = []
               }

{- |
  Function that initialyzes a Yices Solver.
  It Receives a Mode, an SMT Logic, it can receive a diferent configuration
  for the solver and an anternative path to create the script in Script Mode.

  In Online Mode if a FilePath is passed then it's ignored.
-}
startYices :: Mode -> String -> Maybe SolverConfig -> Maybe FilePath -> IO Solver
startYices Slv.Batch logic sConf _ = startYicesBatch logic sConf
startYices Slv.Online logic sConf _ = startYicesOnline logic sConf
startYices Slv.Script logic sConf scriptFilePath =
    startYicesScript logic sConf scriptFilePath



-- Start Yices Online.

startYicesOnline :: String -> Maybe SolverConfig -> IO Solver
startYicesOnline logic Nothing =  startYicesOnline' logic yicesConfigOnline
startYicesOnline logic (Just conf) = startYicesOnline' logic conf

startYicesOnline':: String -> SolverConfig -> IO Solver
startYicesOnline' logic conf = do
  -- Starts a Yices Process.
  process <- beginProcess (path conf) (args conf)
  --Set Option to print success after accepting a Command.
  onlineSetOption process (OptPrintSuccess True)
  -- Sets the SMT Logic.
  onlineSetLogic process (N logic)
  -- Initialize the solver Functions and return them.
  return $ onlineSolver process


--Start Yices Script.

startYicesScript :: String -> Maybe SolverConfig -> Maybe FilePath -> IO Solver
startYicesScript logic Nothing Nothing =
    startYicesScript' logic yicesConfigScript "temp.smt2"
startYicesScript logic (Just conf) Nothing =
    startYicesScript' logic conf "temp.smt2"
startYicesScript  logic Nothing (Just scriptFilePath) =
    startYicesScript' logic yicesConfigScript scriptFilePath
startYicesScript logic (Just conf) (Just scriptFilePath) =
    startYicesScript' logic conf scriptFilePath

{-
  In this function a file is created where the commands are kept.

  Every function in the ScriptCmd Module needs a ScriptConf data which has:

  - sHandle: The handle of the script file
  - sCmdPath: The Path to initilyze the solver
  - sArgs: The options of the solver
  - sFilePath: The file path of the script so it can be passed to the solver
               when started.
-}
startYicesScript' :: String -> SolverConfig -> FilePath -> IO Solver
startYicesScript' logic conf scriptFilePath = do
  -- Create a file with the give file path.
  -- Since the handle is created with WriteMode it overrides a file if it
  -- already exists.
  scriptHandle <- openFile scriptFilePath WriteMode
  -- Creates the arguments for the functions in ScriptCmd
  let srcmd = newScriptArgs conf scriptHandle scriptFilePath
  --Set Option to print success after accepting a Command.
  scriptSetOption srcmd (OptPrintSuccess True)
  -- Initialize the solver Functions and return them.
  scriptSetLogic srcmd (N logic)
  return $ scriptSolver srcmd

--Function which creates the ScriptConf for the script functions.
newScriptArgs :: SolverConfig  -> Handle -> FilePath -> ScriptConf
newScriptArgs solverConfig nHandle scriptFilePath =
  ScriptConf { sHandle = nHandle
             , sCmdPath = path solverConfig
             , sArgs = args solverConfig
             , sFilePath  = scriptFilePath
             }

-- start Yices solver

startYicesBatch :: String -> Maybe SolverConfig -> IO Solver
startYicesBatch logic Nothing = startYicesBatch' logic yicesConfigBatch
startYicesBatch logic (Just conf) = startYicesBatch' logic conf

startYicesBatch' :: String -> SolverConfig -> IO Solver
startYicesBatch' logic conf = return $ batchSolver logic conf


-- Creates the functions for online mode with the process already running.
-- Each function will send the command to the solver and wait for the response.
onlineSolver :: Process -> Solver
onlineSolver process =
  Solver { setLogic = onlineSetLogic process
         , setOption = onlineSetOption process
         , setInfo = onlineSetInfo process
         , declareType = onlineDeclareType process
         , defineType = onlineDefineType process
         , declareFun = onlineDeclareFun process
         , defineFun = onlineDefineFun process
         , push = onlinePush process
         , pop = onlinePop process
         , assert = onlineAssert process
         , checkSat = onlineCheckSat process
         , getAssertions = onlineGetAssertions process
         , getValue = onlineGetValue process
         , getProof = onlineGetProof process
         , getUnsatCore = onlineGetUnsatCore process
         , getInfo = onlineGetInfo process
         , getOption = onlineGetOption process
         , exit = onlineExit process
         }


-- Creates the funtion for the script mode.
-- The configuration of the file is passed.
scriptSolver :: ScriptConf -> Solver
scriptSolver srcmd =
  Solver { setLogic = scriptSetLogic srcmd
         , setOption = scriptSetOption srcmd
         , setInfo = scriptSetInfo srcmd
         , declareType = scriptDeclareType srcmd
         , defineType = scriptDefineType srcmd
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

batchSolver :: String -> SolverConfig -> Solver
batchSolver logic config =
  BSolver { Slv.executeBatch = B.executeBatch (path config) (args config) logic}