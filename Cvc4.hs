module Cvc4(startCvc4) where

import           OnlineCmd
import           Process
import           SmtConfig as Config
import           SMTLib2
import           Solver


cvc4ConfigOnline :: Config
cvc4ConfigOnline =
    Config { path = "cvc4"
           , Config.args = ["--smtlib-strict"] -- with theflag --status the process frezzes, investigate why
           , defaultMode = "Script"
           , avaliableModes = ["Context", "Script"]
           }


startCvc4 :: String -> Mode -> Maybe Config ->  Maybe FilePath -> IO Solver
startCvc4 logic Online config _ = startCvc4Online logic config

startCvc4Online :: String -> Maybe Config -> IO Solver
startCvc4Online logic Nothing =  startCvc4Online' logic cvc4ConfigOnline
startCvc4Online logic (Just conf) = startCvc4Online' logic conf

startCvc4Online':: String -> Config -> IO Solver
startCvc4Online' logic conf = do
  process <- beginProcess (path conf) (Config.args conf)
  onlineSetOption process (OptPrintSuccess True)
  onlineSetLogic process (N logic)
  return $ onlineSolver process


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
