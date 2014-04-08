{- |
Module      : mathSat
  Module wich has the standard configuration for all mathSat Modes and
  provides the initilizing function.
-}
module MathSAT(startmathSat) where

import           Cmd.OnlineCmd
import           Cmd.ProcCom.Process
import           Cmd.ScriptCmd
import           Cmd.Solver          as Slv
import           SMTLib2
import           System.IO           (Handle, IOMode (WriteMode), openFile)

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


{- |
  Function that initialyzes a mathSat Solver.
  It Receives a Mode, an SMT Logic, it can receive a diferent configuration
  for the solver and an anternative path to create the script in Script Mode.

  In Online Mode if a FilePath is passed then it's ignored.
-}
startmathSat :: Mode -> String -> Maybe SolverConfig -> Maybe FilePath -> IO Solver
startmathSat Slv.Online logic sConf _ = startmathSatOnline logic sConf
startmathSat Slv.Script logic sConf scriptFilePath =
    startmathSatScript logic sConf scriptFilePath

-- Start mathSat Online.

startmathSatOnline :: String -> Maybe SolverConfig -> IO Solver
startmathSatOnline logic Nothing = startmathSatOnline' logic mathSatConfigOnline
startmathSatOnline logic (Just conf) = startmathSatOnline' logic conf

startmathSatOnline' :: String -> SolverConfig -> IO Solver
startmathSatOnline' logic conf = do
  -- Starts a Z4 Process.
  process <- beginProcess (path conf) (args conf)
  --Set Option to print success after accepting a Command.
  onlineSetOption process (OptPrintSuccess True)
  -- Sets the SMT Logic.
  onlineSetLogic process (N logic)
  -- Initialize the solver Functions and return them.
  return $ onlineSolver process

--Start mathSat Script.

startmathSatScript :: String -> Maybe SolverConfig -> Maybe FilePath -> IO Solver
startmathSatScript logic Nothing Nothing =
    startmathSatScript' logic mathSatConfigScript "temp.smt2"
startmathSatScript logic (Just conf) Nothing =
    startmathSatScript' logic conf "temp.smt2"
startmathSatScript logic Nothing (Just scriptFilePath) =
    startmathSatScript' logic mathSatConfigScript scriptFilePath
startmathSatScript logic (Just conf) (Just scriptFilePath) =
    startmathSatScript' logic conf scriptFilePath

{-
  In this function a file is created where the commands are kept.

  Every function in the ScriptCmd Module needs a ScriptConf data which has:

  - sHandle: The handle of the script file
  - sCmdPath: The Path to initilyze the solver
  - sArgs: The options of the solver
  - sFilePath: The file path of the script so it can be passed to the solver
               when started.
-}
startmathSatScript' :: String -> SolverConfig -> FilePath -> IO Solver
startmathSatScript' logic conf scriptFilePath = do
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
