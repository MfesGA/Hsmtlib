{- |
Module      : boolector
  Module wich has the standard configuration for all boolector Modes and
  provides the initilizing function.
-}
module Boolector(startboolector) where

import           Cmd.OnlineCmd
import           Cmd.ProcCom.Process
import           Cmd.ScriptCmd
import           Cmd.Solver          as Slv
import           SMTLib2
import           System.IO

-- All the configurations are the same but have diferent names so if anything
-- changes it's easy to alter its configuration.


boolectorConfigOnline :: SolverConfig
boolectorConfigOnline =
        Config { path = "boolector"
               , args = ["--smt2", "-d2", "-o STDOUT"]
               }

boolectorConfigScript :: SolverConfig
boolectorConfigScript =
        Config { path = "boolector"
               , args = ["--smt2", "-d2", "-o STDOUT"]
               }



{- |
  Function that initialyzes a boolector Solver.
  It Receives a Mode, an SMT Logic, it can receive a diferent configuration
  for the solver and an anternative path to create the script in Script Mode.

  In Online Mode if a FilePath is passed then it's ignored.
-}
startboolector :: Mode -> String -> Maybe SolverConfig -> Maybe FilePath -> IO Solver
startboolector Slv.Online logic sConf _ = startboolectorOnline logic sConf
startboolector Slv.Script logic sConf scriptFilePath =
    startboolectorScript logic sConf scriptFilePath

-- Start boolector Online.

startboolectorOnline :: String -> Maybe SolverConfig -> IO Solver
startboolectorOnline logic Nothing = startboolectorOnline' logic boolectorConfigOnline
startboolectorOnline logic (Just conf) = startboolectorOnline' logic conf

startboolectorOnline' :: String -> SolverConfig -> IO Solver
startboolectorOnline' logic conf = do
  -- Starts a Z4 Process.
  process <- beginProcess (path conf) (args conf)
  --Set Option to print success after accepting a Command.
  --onlineSetOption process (OptPrintSuccess True)
  -- Sets the SMT Logic.
  onlineSetLogic process (N logic)
  -- Initialize the solver Functions and return them.
  return $ onlineSolver process

--Start boolector Script.

startboolectorScript :: String -> Maybe SolverConfig -> Maybe FilePath -> IO Solver
startboolectorScript logic Nothing Nothing =
    startboolectorScript' logic boolectorConfigScript "temp.smt2"
startboolectorScript logic (Just conf) Nothing =
    startboolectorScript' logic conf "temp.smt2"
startboolectorScript logic Nothing (Just scriptFilePath) =
    startboolectorScript' logic boolectorConfigScript scriptFilePath
startboolectorScript logic (Just conf) (Just scriptFilePath) =
    startboolectorScript' logic conf scriptFilePath

{-
  In this function a file is created where the commands are kept.

  Every function in the ScriptCmd Module needs a ScriptConf data which has:

  - sHandle: The handle of the script file
  - sCmdPath: The Path to initilyze the solver
  - sArgs: The options of the solver
  - sFilePath: The file path of the script so it can be passed to the solver
               when started.
-}
startboolectorScript' :: String -> SolverConfig -> FilePath -> IO Solver
startboolectorScript' logic conf scriptFilePath = do
  scriptHandle <- openFile scriptFilePath WriteMode
  let srcmd = newScriptArgs conf scriptHandle scriptFilePath
  scriptSetOption srcmd (OptPrintSuccess True)
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
