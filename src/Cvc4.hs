{- |
Module      : Cvc4
  Module wich has the standard configuration for all Cvc4 Modes and
  provides the initilizing function.
-}
module Cvc4(startCvc4) where


import           Cmd.BatchCmd        as B (executeBatch)
import           Cmd.OnlineCmd
import           Cmd.ProcCom.Process
import           Cmd.ScriptCmd
import           Cmd.Solver          as Slv
import           SMTLib2
import           System.IO           (Handle, IOMode (WriteMode), openFile)

{-
    TODO: Why the flag --status frezzes the process.
-}

cvc4ConfigOnline :: SolverConfig
cvc4ConfigOnline =
    Config { path = "cvc4"
           , args = ["--interactive", "--lang=smt2", "--quiet"]
           }

-- Both Script configurations are the same but have diferent names
-- so if anything  changes it's easy to alter its configuration.

cvc4ConfigScript :: SolverConfig
cvc4ConfigScript =
    Config { path = "cvc4"
           , args = ["--lang=smt2"]
           }

cvc4ConfigBatch :: SolverConfig
cvc4ConfigBatch =
        Config { path = "cvc4"
               , args = ["--lang=smt2"]
               }

{- |
  Function that initialyzes a Cvc4 Solver.
  It Receives a Mode, an SMT Logic, it can receive a diferent configuration
  for the solver and an anternative path to create the script in Script Mode.

  In Context and Online Mode if a FilePath is passed then it's ignored.
-}
startCvc4 :: Mode -> String -> Maybe SolverConfig -> Maybe FilePath -> IO Solver
startCvc4 Slv.Batch logic sConf _ = startCvc4Batch logic sConf
startCvc4 Slv.Online logic sConf _ = startCvc4Online logic sConf
startCvc4 Slv.Script logic sConf scriptFilePath =
    startCvc4Script logic sConf scriptFilePath



-- Start Cvc4 Online.

startCvc4Online :: String -> Maybe SolverConfig -> IO Solver
startCvc4Online logic Nothing =  startCvc4Online' logic cvc4ConfigOnline
startCvc4Online logic (Just conf) = startCvc4Online' logic conf

startCvc4Online':: String -> SolverConfig -> IO Solver
startCvc4Online' logic conf = do
  -- Starts a Cvc4 Process.
  process <- beginProcess (path conf) (args conf)
  --Set Option to print success after accepting a Command.
  onlineSetOption process (OptPrintSuccess True)
  -- Sets the SMT Logic.
  onlineSetLogic process (N logic)
  -- Initialize the solver Functions and return them.
  return $ onlineSolver process


--Start Cvc4 Script.

startCvc4Script :: String -> Maybe SolverConfig -> Maybe FilePath -> IO Solver
startCvc4Script logic Nothing Nothing =
    startCvc4Script' logic cvc4ConfigScript "temp.smt2"
startCvc4Script logic (Just conf) Nothing =
    startCvc4Script' logic conf "temp.smt2"
startCvc4Script  logic Nothing (Just scriptFilePath) =
    startCvc4Script' logic cvc4ConfigScript scriptFilePath
startCvc4Script logic (Just conf) (Just scriptFilePath) =
    startCvc4Script' logic conf scriptFilePath

{-
  In this function a file is created where the commands are kept.

  Every function in the ScriptCmd Module needs a ScriptConf data which has:

  - sHandle: The handle of the script file
  - sCmdPath: The Path to initilyze the solver
  - sArgs: The options of the solver
  - sFilePath: The file path of the script so it can be passed to the solver
               when started.
-}
startCvc4Script' :: String -> SolverConfig -> FilePath -> IO Solver
startCvc4Script' logic conf scriptFilePath = do
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


-- start Cvc4 Batch
startCvc4Batch :: String -> Maybe SolverConfig -> IO Solver
startCvc4Batch logic Nothing = startCvc4Batch' logic cvc4ConfigBatch
startCvc4Batch logic (Just conf) = startCvc4Batch' logic conf

startCvc4Batch' :: String -> SolverConfig -> IO Solver
startCvc4Batch' logic conf = return $ batchSolver logic conf



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
