module Z3(startZ3) where

import           OnlineCmd
import           Process
import           ScriptCmd as Scmd
import           SmtConfig as Config
import           SMTLib2
import           Solver
import           System.IO

z3Config :: Config
z3Config =
  Config { path = "z3"
         , Config.args = ["-smt2","-in"]
         , defaultMode = "Online"
         , avaliableModes = ["Online", "Context", "Script"]
         }

z3ConfigScript :: Config
z3ConfigScript =
  Config { path = "z3"
         , Config.args = ["-smt2"]
         , defaultMode = "Online"
         , avaliableModes = ["Online", "Context", "Script"]
         }

startZ3 :: String -> Mode -> Maybe Config-> Maybe FilePath -> IO Solver
startZ3 logic Online config _ = startZ3Online logic config
startZ3 logic Config.Script config (Just sFilePath) = startZ3Script logic config sFilePath

startZ3Online :: String -> Maybe Config -> IO Solver
startZ3Online logic Nothing = startZ3Online' logic z3Config
startZ3Online logic (Just conf) = startZ3Online' logic conf

startZ3Online' :: String -> Config -> IO Solver
startZ3Online' logic conf = do
  process <- beginProcess (path conf) (Config.args conf)
  onlineSetOption process (OptPrintSuccess True)
  onlineSetLogic process (N logic)
  return $ onlineSolver process

startZ3Script :: String -> Maybe Config -> FilePath -> IO Solver
startZ3Script  logic Nothing sFilePath = startZ3Script' logic z3ConfigScript sFilePath
startZ3Script logic (Just conf) sFilePath = startZ3Script' logic conf sFilePath

startZ3Script' :: String -> Config -> FilePath -> IO Solver
startZ3Script' logic conf sFilePath = do
  sHandle <- openFile sFilePath WriteMode
  let srcmd = newScriptArgs conf sHandle sFilePath
  scriptSetOption srcmd (OptPrintSuccess True)
  scriptSetLogic srcmd (N logic)
  return $ scriptSolver sHandle srcmd

newScriptArgs :: Config -> Handle -> FilePath -> ScriptArgs
newScriptArgs config nHandle sFilePath =
  ScriptArgs { handle = nHandle
             , cmdPath = path config
             , Scmd.args = Config.args config
             , filePath  = sFilePath
             }


onlineSolver :: Process -> Solver
onlineSolver process =
  Solver { setLogic = onlineSetLogic process
         , setOption = onlineSetOption process
         , setInfo = onlineSetInfo process
         , declareType = onlineDeclareType process
         , defineType = onlineDefineType process
         , declareFun = onlineDeclareFun process
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

scriptSolver :: Handle -> ScriptArgs -> Solver
scriptSolver sHandle srcmd =
  Solver { setLogic = scriptSetLogic srcmd
         , setOption = scriptSetOption srcmd
         , setInfo = scriptSetInfo srcmd
         , declareType = scriptDeclareType srcmd
         , defineType = scriptDefineType srcmd
         , declareFun = scriptDeclareFun srcmd
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
         , exit = scriptExit sHandle srcmd
         }

