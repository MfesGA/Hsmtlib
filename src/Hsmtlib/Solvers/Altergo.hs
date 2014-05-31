{- |
Module      : Hsmtlib.Solvers.Altergo
  Module wich has the standard configuration for all altergo Modes and
  provides the initilizing function.
-}
module Hsmtlib.Solvers.Altergo(startAltErgo) where

import           Hsmtlib.Solver                      as Slv
import           Hsmtlib.Solvers.Cmd.BatchCmd        as B (executeBatch)
import           Hsmtlib.Solvers.Cmd.OnlineCmd
import           Hsmtlib.Solvers.Cmd.ProcCom.Process
import           Hsmtlib.Solvers.Cmd.ScriptCmd
import           SMTLib2
import           System.IO                           (Handle,
                                                      IOMode (WriteMode),
                                                      openFile)


-- All the configurations are the same but have diferent names so if anything
-- changes it's easy to alter its configuration.


altErgoConfigOnline :: SolverConfig
altErgoConfigOnline =
        Config { path = "altergo"
               , args = ["-v"]
               }

altErgoConfigScript :: SolverConfig
altErgoConfigScript =
        Config { path = "altergo"
               , args = ["-v"]
               }

altErgoConfigBatch :: SolverConfig
altErgoConfigBatch =
        Config { path = "altergo"
               , args = ["-v"]
               }



{- |
  Function that initialyzes a altergo Solver.
  It Receives a Mode, an SMT Logic, it can receive a diferent configuration
  for the solver and an anternative path to create the script in Script Mode.

  In Online Mode if a FilePath is passed then it's ignored.
-}
startAltErgo :: Mode
             -> String
             -> Maybe SolverConfig
             -> Maybe FilePath
             -> IO Solver

startAltErgo Slv.Batch logic sConf _ = startAltErgoBatch logic sConf
startAltErgo Slv.Online logic sConf _ = startAltErgoOnline logic sConf
startAltErgo Slv.Script logic sConf scriptFilePath =
    startAltErgoScript logic sConf scriptFilePath

-- Start altergo Online.

startAltErgoOnline :: String -> Maybe SolverConfig -> IO Solver
startAltErgoOnline logic Nothing =
  startAltErgoOnline' logic altErgoConfigOnline
startAltErgoOnline logic (Just conf) = startAltErgoOnline' logic conf

startAltErgoOnline' :: String -> SolverConfig -> IO Solver
startAltErgoOnline' logic conf = do
  -- Starts a Z4 Process.
  process <- beginProcess (path conf) (args conf)
  --Set Option to print success after accepting a Command.
  _ <- onlineSetOption Altergo process (OptPrintSuccess True)
  -- Sets the SMT Logic.
  _ <- onlineSetLogic  Altergo process (N logic)
  -- Initialize the solver Functions and return them.
  return $ onlineSolver process

--Start altergo Script.

startAltErgoScript :: String -> Maybe SolverConfig -> Maybe FilePath -> IO Solver
startAltErgoScript logic Nothing Nothing =
    startAltErgoScript' logic altErgoConfigScript "temp.smt2"
startAltErgoScript logic (Just conf) Nothing =
    startAltErgoScript' logic conf "temp.smt2"
startAltErgoScript logic Nothing (Just scriptFilePath) =
    startAltErgoScript' logic altErgoConfigScript scriptFilePath
startAltErgoScript logic (Just conf) (Just scriptFilePath) =
    startAltErgoScript' logic conf scriptFilePath

{-
  In this function a file is created where the commands are kept.

  Every function in the ScriptCmd Module needs a ScriptConf data which has:

  - sHandle: The handle of the script file
  - sCmdPath: The Path to initilyze the solver
  - sArgs: The options of the solver
  - sFilePath: The file path of the script so it can be passed to the solver
               when started.
-}
startAltErgoScript' :: String -> SolverConfig -> FilePath -> IO Solver
startAltErgoScript' logic conf scriptFilePath = do
  scriptHandle <- openFile scriptFilePath WriteMode
  let srcmd = newScriptArgs conf scriptHandle scriptFilePath
  _ <- scriptSetOption srcmd (OptPrintSuccess True)
  _ <- scriptSetLogic srcmd (N logic)
  return $ scriptSolver srcmd

--Function which creates the ScriptConf for the script functions.
newScriptArgs :: SolverConfig  -> Handle -> FilePath -> ScriptConf
newScriptArgs solverConfig nHandle scriptFilePath =
  ScriptConf { sHandle = nHandle
             , sCmdPath = path solverConfig
             , sArgs = args solverConfig
             , sFilePath  = scriptFilePath
             }

-- start Alt-Ergo Batch
startAltErgoBatch :: String -> Maybe SolverConfig -> IO Solver
startAltErgoBatch logic Nothing = startAltErgoBatch' logic altErgoConfigBatch
startAltErgoBatch logic (Just conf) = startAltErgoBatch' logic conf

startAltErgoBatch' :: String -> SolverConfig -> IO Solver
startAltErgoBatch' logic config = return $ batchSolver logic config


-- Creates the functions for online mode with the process already running.
-- Each function will send the command to the solver and wait for the response.
onlineSolver :: Process -> Solver
onlineSolver process =
  Solver { setLogic = onlineSetLogic Altergo process
         , setOption = onlineSetOption Altergo process
         , setInfo = onlineSetInfo Altergo process
         , declareType = onlineDeclareType Altergo process
         , defineType = onlineDefineType Altergo process
         , declareFun = onlineDeclareFun Altergo process
         , defineFun = onlineDefineFun Altergo process
         , push = onlinePush Altergo process
         , pop = onlinePop Altergo process
         , assert = onlineAssert Altergo process
         , checkSat = onlineCheckSat Altergo process
         , getAssertions = onlineGetAssertions Altergo process
         , getValue = onlineGetValue Altergo process
         , getProof = onlineGetProof Altergo process
         , getUnsatCore = onlineGetUnsatCore Altergo process
         , getInfo = onlineGetInfo Altergo process
         , getOption = onlineGetOption Altergo process
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

batchSolver :: String -> SolverConfig  -> Solver
batchSolver logic config =
  BSolver { Slv.executeBatch = B.executeBatch (path config) (args config) logic}
