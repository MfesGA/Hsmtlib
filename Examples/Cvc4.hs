{- |
Module      : Cvc4
  Module wich has the standard configuration for all Cvc4 Modes and
  provides the initilizing function.
-}
module Cvc4(startCvc4) where


import           Cmd.ContextCmd
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

-- Both Script and Context configurations are the same but have diferent names
-- so if anything  changes it's easy to alter its configuration.

cvc4ConfigScript :: SolverConfig
cvc4ConfigScript =
    Config { path = "cvc4"
           , args = ["--lang=smt2"]
           }

cvc4ConfigContext :: SolverConfig
cvc4ConfigContext =
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
startCvc4 Slv.Online logic sConf _ = startCvc4Online logic sConf
startCvc4 Slv.Context logic sConf _ = startCvc4Context logic sConf
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


-- Start Cvc4 Context.


startCvc4Context :: String -> Maybe SolverConfig -> IO Solver
startCvc4Context logic Nothing = startCvc4Context' logic cvc4ConfigContext
startCvc4Context logic (Just conf) = startCvc4Context' logic conf


startCvc4Context' :: String -> SolverConfig -> IO Solver
startCvc4Context' logic conf =
    return $ ctxSolver logic (path conf) (args conf)


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

-- Creates the functions for the context mode.
-- It receives the logic, path of the solver and its arguments.
ctxSolver :: String -> CmdPath -> Args -> Solver
ctxSolver logic cmd solvArgs =
  CtSolver { setLogicCt = ctxSetLogic
           , setOptionCt = ctxSetOption
           , setInfoCt = ctxSetInfo
           , declareTypeCt = ctxDeclareType logic
           , defineTypeCt = ctxDefineType logic
           , defineFunCt = ctxDefineFun logic
           , declareFunCt = ctxDeclareFun logic
           , pushCt = ctxPush
           , popCt = ctxPop
           , assertCt = ctxAssert
           , checkSatCt = ctxCheckSat cmd solvArgs
           , getAssertionsCt = ctxGetAssertions cmd solvArgs
           , getValueCt = ctxGetValue cmd solvArgs
           , getProofCt = ctxGetProof cmd solvArgs
           , getUnsatCoreCt = ctxGetUnsatCore cmd solvArgs
           , getInfoCt = ctxGetInfo cmd solvArgs
           , getOptionCt = ctxGetOption cmd solvArgs
           , exitCt = ctxExit cmd solvArgs
           }