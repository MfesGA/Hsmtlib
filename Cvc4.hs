module Cvc4(startCvc4) where

import           OnlineCmd
import           Process
import           SmtConfig as Config
import           SMTLib2
import           Solver
import           ScriptCmd as Scmd
import           System.IO


cvc4ConfigOnline :: Config
cvc4ConfigOnline =
    Config { path = "cvc4"
           , Config.args = ["--smtlib-strict", "--interactive", "--quiet"] -- with theflag --status the process frezzes, investigate why the flag --interactive forces interactive cvc4 but it does not terminate
           , defaultMode = "Script"
           , avaliableModes = ["Context", "Script"]
           }

cvc4ConfigScript :: Config
cvc4ConfigScript = cvc4ConfigOnline --its the same for now if it ever changes its already separeted

cvc4ConfigContext :: Config
cvc4ConfigContext = cvc4ConfigOnline -- the same as above


startCvc4 :: String -> Mode -> Maybe Config ->  Maybe FilePath -> IO Solver
startCvc4 logic Online config _ = startCvc4Online logic config
startCvc4 logic Config.Script config (Just path) = startCvc4Script logic config path

startCvc4Online :: String -> Maybe Config -> IO Solver
startCvc4Online logic Nothing =  startCvc4Online' logic cvc4ConfigOnline
startCvc4Online logic (Just conf) = startCvc4Online' logic conf

startCvc4Online':: String -> Config -> IO Solver
startCvc4Online' logic conf = do
  process <- beginProcess (path conf) (Config.args conf)
  onlineSetOption process (OptPrintSuccess True)
  onlineSetLogic process (N logic)
  return $ onlineSolver process

newScriptArgs :: Config -> Handle -> FilePath -> ScriptArgs
newScriptArgs config nHandle sFilePath =
  ScriptArgs { handle = nHandle
             , cmdPath = path config
             , Scmd.args = Config.args config
             , filePath  = sFilePath
             }


startCvc4Script :: String -> Maybe Config -> FilePath -> IO Solver
startCvc4Script  logic Nothing sFilePath = startCvc4Script' logic cvc4ConfigScript sFilePath --to do confscrip
startCvc4Script logic (Just conf) sFilePath = startCvc4Script' logic conf sFilePath

startCvc4Script' :: String -> Config -> FilePath -> IO Solver
startCvc4Script' logic conf sFilePath = do
  sHandle <- openFile sFilePath WriteMode
  let srcmd = newScriptArgs conf sHandle sFilePath
  scriptSetOption srcmd (OptPrintSuccess True)
  scriptSetLogic srcmd (N logic)
  return $ scriptSolver sHandle srcmd

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
