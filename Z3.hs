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
                  
z3ConfigScript :: Config
z3ConfigScript = Config 
                  { path = "z3"
                  ,  Config.args = ["-smt2"]
                  , defaultMode = "Online"
                  , avaliableModes = ["Online", "Context", "Script"]
                  }
startZ3 :: String -> Mode -> Maybe Config-> Maybe FilePath -> IO Solver
startZ3 logic Online config _ = startZ3Online logic config
startZ3 logic Config.Script config (Just filePath) = startZ3Script logic config filePath

startZ3Online :: String -> Maybe Config -> IO Solver
startZ3Online logic Nothing = do
    process <- beginProcess (path z3Config) (Config.args z3Config) 
    onlineSetOption process (OptPrintSuccess True)
    onlineSetLogic process (N logic)
    return $ onlineSolver process
startZ3Online logic (Just conf) = do
    process <- beginProcess (path conf) (Config.args conf)
    onlineSetOption process (OptPrintSuccess True)
    onlineSetLogic process (N logic)
    return $ onlineSolver process

startZ3Script :: String -> Maybe Config -> FilePath -> IO Solver
startZ3Script  logic Nothing filePath = do
    handle <- openFile filePath WriteMode
    let srcmd = newSrcmd z3ConfigScript handle filePath
    scriptSetOption srcmd (OptPrintSuccess True)
    scriptSetLogic srcmd (N logic)
    return $ scriptSolver handle srcmd

startZ3Script logic (Just conf) filePath = do
    handle <- openFile filePath WriteMode
    let srcmd = newSrcmd conf handle filePath
    scriptSetOption srcmd (OptPrintSuccess True)
    scriptSetLogic srcmd (N logic)
    return $ scriptSolver handle srcmd

newSrcmd :: Config -> Handle -> FilePath -> Srcmd
newSrcmd config handle filePath = Srcmd { handle = handle
                                                                   , cmdPath = path config
                                                                   , Cmd.args = Config.args config
                                                                   , filePath  = filePath
                                                                   }


onlineSolver :: Process -> Solver
onlineSolver process = Solver {  setLogic = onlineSetLogic process 
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

scriptSolver :: Handle -> Srcmd -> Solver
scriptSolver handle srcmd = Solver { setLogic = scriptSetLogic srcmd 
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
                                            , exit = scriptExit handle srcmd
                                            }



{--
startZ3Script :: String  ->   IO Solver
startZ3Script logic = do
    handle <- openFile "script.smt2" AppendMode
    let srcmd = Srcmd{ handle = handle
                                    , cmdPath = path z3Config
                                    , Cmd.args = ["-smt2"]
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
                            , exit = scriptExit process handle srcmd
                            }
--}



{--
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
--}