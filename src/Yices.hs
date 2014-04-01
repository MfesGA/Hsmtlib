{- |
Module      : Yices
  Module wich has the standard configuration for all Yices Modes and
  provides the initilizing function.
-}
module Yices(startYices) where


import           Cmd.ContextCmd
import           Cmd.OnlineCmd
import           Cmd.ProcCom.Process
import           Cmd.ScriptCmd
import           Cmd.Solver          as Slv
import           SMTLib2
import           System.IO           (Handle, IOMode (WriteMode), openFile)


yicesConfigOnline :: SolverConfig
yicesConfigOnline =
    Config { path = "yices"
           , args = ["-smt"]
           }

-- Both Script and Context configurations are the same but have diferent names
-- so if anything  changes it's easy to alter its configuration.

yicesConfigScript :: SolverConfig
yicesConfigScript =
    Config { path = "yices"
           , args = ["-smt"]
           }

yicesConfigContext :: SolverConfig
yicesConfigContext =
    Config { path = "yices"
           , args = ["-smt"]
           }

{- |
  Function that initialyzes a Yices Solver.
  It Receives a Mode, an SMT Logic, it can receive a diferent configuration
  for the solver and an anternative path to create the script in Script Mode.

  In Context and Online Mode if a FilePath is passed then it's ignored.
-}
startYices :: Mode -> String -> Maybe SolverConfig -> Maybe FilePath -> IO Solver
startYices Slv.Online logic sConf _ = startYicesOnline logic sConf
startYices Slv.Context logic sConf _ = startYicesContext logic sConf
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


-- Start Yices Context.


startYicesContext :: String -> Maybe SolverConfig -> IO Solver
startYicesContext logic Nothing = startYicesContext' logic yicesConfigContext
startYicesContext logic (Just conf) = startYicesContext' logic conf


startYicesContext' :: String -> SolverConfig -> IO Solver
startYicesContext' logic conf =
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
