{- |
Module      : Hsmtlib.Solvers.Z3
  Module wich has the standard configuration for all Z3 Modes and
  provides the initilizing function.
-}
module Hsmtlib.Solvers.Z3(startZ3) where

import           Control.Applicative                  (liftA)
import           Hsmtlib.Solver                      as Slv
import           Hsmtlib.Solvers.Cmd.BatchCmd        as B (executeBatch)
import           Hsmtlib.Solvers.Cmd.OnlineCmd
import           Hsmtlib.Solvers.Cmd.ProcCom.Process
import           Hsmtlib.Solvers.Cmd.ScriptCmd
import           Hsmtlib.Solvers.Cmd.CmdResult
import           SMTLib2
import           System.IO                           (Handle,
                                                      IOMode (WriteMode),
                                                      openFile)

-- All the configurations are the same but have diferent names so if anything
-- changes it's easy to alter its configuration.

z3ConfigOnline :: SolverConfig
z3ConfigOnline =
        Config { path = "z3"
               , args = ["-smt2","-in"]
               }

z3ConfigScript :: SolverConfig
z3ConfigScript =
        Config { path = "z3"
               , args = ["-smt2"]
               }

z3ConfigBatch :: SolverConfig
z3ConfigBatch =
        Config { path = "z3"
               , args = ["-smt2"]
               }

{- |
  Function that initialyzes a Z3 Solver.
  It Receives a Mode, an SMT Logic, it can receive a diferent configuration
  for the solver and an anternative path to create the script in Script Mode.

  In Online Mode if a FilePath is passed then it's ignored.
-}
startZ3 :: Mode -> String -> Maybe SolverConfig -> Maybe FilePath -> IO Solver
startZ3 Slv.Batch logic sConf _ = startZ3Batch logic sConf
startZ3 Slv.Online logic sConf _ = startZ3Online logic sConf
startZ3 Slv.Script logic sConf scriptFilePath =
    startZ3Script logic sConf scriptFilePath

-- Start Z3 Online.

startZ3Online :: String -> Maybe SolverConfig -> IO Solver
startZ3Online logic Nothing = startZ3Online' logic z3ConfigOnline
startZ3Online logic (Just conf) = startZ3Online' logic conf

startZ3Online' :: String -> SolverConfig -> IO Solver
startZ3Online' logic conf = do
  -- Starts a Z3 Process.
  process <- beginProcess (path conf) (args conf)
  --Set Option to print success after accepting a Command.
  onlineSetOption Z3 process (OptPrintSuccess True)
  -- Sets the SMT Logic.
  onlineSetLogic Z3 process (N logic)
  -- Initialize the solver Functions and return them.
  return $ onlineSolver process

--Start Z3 Script.

startZ3Script :: String -> Maybe SolverConfig -> Maybe FilePath -> IO Solver
startZ3Script logic Nothing Nothing =
    startZ3Script' logic z3ConfigScript "temp.smt2"
startZ3Script logic (Just conf) Nothing =
    startZ3Script' logic conf "temp.smt2"
startZ3Script logic Nothing (Just scriptFilePath) =
    startZ3Script' logic z3ConfigScript scriptFilePath
startZ3Script logic (Just conf) (Just scriptFilePath) =
    startZ3Script' logic conf scriptFilePath

{-
  In this function a file is created where the commands are kept.

  Every function in the ScriptCmd Module needs a ScriptConf data which has:

  - sHandle: The handle of the script file
  - sCmdPath: The Path to initilyze the solver
  - sArgs: The options of the solver
  - sFilePath: The file path of the script so it can be passed to the solver
               when started.
-}
startZ3Script' :: String -> SolverConfig -> FilePath -> IO Solver
startZ3Script' logic conf scriptFilePath = do
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


-- Start z3 batch mode

startZ3Batch :: String -> Maybe SolverConfig -> IO Solver
startZ3Batch logic Nothing = startZ3Batch' logic z3ConfigBatch
startZ3Batch logic (Just conf) = startZ3Batch' logic conf

startZ3Batch' :: String -> SolverConfig -> IO Solver
startZ3Batch' logic conf = return $ batchSolver logic conf




-- Creates the functions for online mode with the process already running.
-- Each function will send the command to the solver and wait for the response.
onlineSolver :: Process -> Solver
onlineSolver process =
  Solver { setLogic = onlineSetLogic Z3 process
         , setOption = onlineSetOption Z3  process
         , setInfo = onlineSetInfo Z3  process
         , declareType = onlineDeclareType Z3  process
         , defineType = onlineDefineType Z3  process
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
{-Specific parse result functions that works only in case of Z3 because it breaks the result with new line and not space-}
scriptGetValueResponseZ3 :: ScriptConf  -> Command -> IO Result
scriptGetValueResponseZ3 conf cmd =
  liftA getValueResponse (scriptFunExecZ3 conf cmd)

scriptFunExecZ3 :: ScriptConf -> Command -> IO String
scriptFunExecZ3 sConf cmd = do
  writeToScript sConf cmd
  res <- sendScript (sCmdPath sConf) (sArgs sConf) (sFilePath sConf)
  return $ unlines $ drop 1 $  snd $ break (==['s','a','t']) $ lines $ res

scriptGetValueZ3 :: ScriptConf -> [Expr] -> IO Result
scriptGetValueZ3 sConf exprs = scriptGetValueResponseZ3 sConf (CmdGetValue exprs)

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
         , getValue = scriptGetValueZ3 srcmd
         , getProof = scriptGetProof srcmd
         , getUnsatCore = scriptGetUnsatCore srcmd
         , getInfo = scriptGetInfo srcmd
         , getOption = scriptGetOption srcmd
         , exit = scriptExit srcmd
         }

batchSolver :: String -> SolverConfig -> Solver
batchSolver logic config =
  BSolver { Slv.executeBatch = B.executeBatch (path config) (args config) logic}
