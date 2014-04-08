{- |
Module      : altergo
  Module wich has the standard configuration for all altergo Modes and
  provides the initilizing function.
-}
module Altergo(startaltergo) where

import           Cmd.ContextCmd
import           Cmd.OnlineCmd
import           Cmd.ProcCom.Process
import           Cmd.ScriptCmd
import           Cmd.Solver          as Slv
import           SMTLib2
import           System.IO

-- All the configurations are the same but have diferent names so if anything
-- changes it's easy to alter its configuration.


altergoConfigOnline :: SolverConfig
altergoConfigOnline =
        Config { path = "altergo"
               , args = ["-v"]
               }

altergoConfigScript :: SolverConfig
altergoConfigScript =
        Config { path = "altergo"
               , args = ["-v"]
               }

altergoConfigContext :: SolverConfig
altergoConfigContext =
         Config { path = "altergo"
                , args = ["-v"]
                }

{- |
  Function that initialyzes a altergo Solver.
  It Receives a Mode, an SMT Logic, it can receive a diferent configuration
  for the solver and an anternative path to create the script in Script Mode.

  In Context and Online Mode if a FilePath is passed then it's ignored.
-}
startaltergo :: Mode -> String -> Maybe SolverConfig -> Maybe FilePath -> IO Solver
startaltergo Slv.Context logic sConf _ = startaltergoContext logic sConf
startaltergo Slv.Online logic sConf _ = startaltergoOnline logic sConf
startaltergo Slv.Script logic sConf scriptFilePath =
    startaltergoScript logic sConf scriptFilePath

-- Start altergo Online.

startaltergoOnline :: String -> Maybe SolverConfig -> IO Solver
startaltergoOnline logic Nothing = startaltergoOnline' logic altergoConfigOnline
startaltergoOnline logic (Just conf) = startaltergoOnline' logic conf

startaltergoOnline' :: String -> SolverConfig -> IO Solver
startaltergoOnline' logic conf = do
  -- Starts a Z4 Process.
  process <- beginProcess (path conf) (args conf)
  --Set Option to print success after accepting a Command.
  onlineSetOption process (OptPrintSuccess True)
  -- Sets the SMT Logic.
  onlineSetLogic process (N logic)
  -- Initialize the solver Functions and return them.
  return $ onlineSolver process

--Start altergo Script.

startaltergoScript :: String -> Maybe SolverConfig -> Maybe FilePath -> IO Solver
startaltergoScript logic Nothing Nothing =
    startaltergoScript' logic altergoConfigScript "temp.smt2"
startaltergoScript logic (Just conf) Nothing =
    startaltergoScript' logic conf "temp.smt2"
startaltergoScript logic Nothing (Just scriptFilePath) =
    startaltergoScript' logic altergoConfigScript scriptFilePath
startaltergoScript logic (Just conf) (Just scriptFilePath) =
    startaltergoScript' logic conf scriptFilePath

{-
  In this function a file is created where the commands are kept.

  Every function in the ScriptCmd Module needs a ScriptConf data which has:

  - sHandle: The handle of the script file
  - sCmdPath: The Path to initilyze the solver
  - sArgs: The options of the solver
  - sFilePath: The file path of the script so it can be passed to the solver
               when started.
-}
startaltergoScript' :: String -> SolverConfig -> FilePath -> IO Solver
startaltergoScript' logic conf scriptFilePath = do
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


-- Start altergo Context.


startaltergoContext :: String -> Maybe SolverConfig -> IO Solver
startaltergoContext logic Nothing = startaltergoContext' logic altergoConfigContext
startaltergoContext logic (Just conf) = startaltergoContext' logic conf

startaltergoContext' :: String -> SolverConfig -> IO Solver
startaltergoContext' logic conf = return $ ctxSolver logic (path conf) (args conf)


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
           , declareFunCt = ctxDeclareFun logic
           , defineFunCt = ctxDefineFun logic
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
