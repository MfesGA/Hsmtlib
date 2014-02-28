module Z3(startZ3) where

import  Solver
import SMTLib2
import SmtConfig as Config
import OnlineCmd
import ScriptCmd as Cmd
import System.IO
import Process

z3Config :: Config
z3Config = Config 
                  { path = "z3"
                  ,  Config.args = ["-smt2","-in"]
                  , defaultMode = "Online"
                  , avaliableModes = ["Online", "Context", "Script"]
                  }




startZ3 :: String -> Maybe Config-> IO Solver
startZ3 logic Nothing = startZ3Online logic
startZ3 logic (Just config) = 
    case defaultMode config  of
      "Online"  -> startZ3Online logic
      "Script" -> startZ3Script logic

startZ3Online :: String  ->  IO Solver
startZ3Online logic = do
    process <- beginProcess (path z3Config) (Config.args z3Config)
    onlineSetOption process (OptPrintSuccess True) >>= print
    onlineSetLogic process (N logic) >>= print
    return Solver {  setLogic = onlineSetLogic process 
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

startZ3Script :: String  ->  IO Solver
startZ3Script logic = do
    process <- beginProcess (path z3Config) (Config.args z3Config)
    handle <- openFile "script.smtl2" AppendMode
    let srcmd = Srcmd{ handle = handle
                                    , cmdPath = path z3Config
                                    , Cmd.args = Config.args z3Config
                                    , filePath = "script.smt2"
                                  }
    scriptSetOption process srcmd (OptPrintSuccess True) >>= print
    scriptSetLogic process srcmd (N logic) >>= print
    return Solver { setLogic = scriptSetLogic process srcmd 
                            , setOption = scriptSetOption process srcmd
                            , setInfo = scriptSetInfo process srcmd
                            , declareType = scriptDeclareType process srcmd
                            , defineType = scriptDefineType process srcmd
                            , declareFun = scriptDeclareFun process srcmd
                            , push = scriptPush process srcmd
                            , pop = scriptPop process srcmd
                            , assert = scriptAssert process srcmd
                            , checkSat = scriptCheckSat process srcmd
                            , getAssertions = scriptGetAssertions process srcmd
                            , getValue = scriptGetValue process srcmd
                            , getProof = scriptGetProof process srcmd
                            , getUnsatCore = scriptGetUnsatCore process srcmd
                            , getInfo = scriptGetInfo process srcmd
                            , getOption = scriptGetOption process srcmd
                            , exit = scriptExit process srcmd
                            }