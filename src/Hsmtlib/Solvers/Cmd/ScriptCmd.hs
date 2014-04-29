{- |
Module      : ScriptCmd

Module with the functions used in script Mode.
-}
module Hsmtlib.Solvers.Cmd.ScriptCmd where

import           Control.Applicative                  (liftA)
import           Hsmtlib.Solver
import           Hsmtlib.Solvers.Cmd.Parser.CmdResult
import           Hsmtlib.Solvers.Cmd.ProcCom.Process
import           SMTLib2
import           System.IO                            (Handle, hClose, hFlush,
                                                       hPutStr)
import           Text.PrettyPrint

-- Data that hass the arguments for the script
data ScriptConf = ScriptConf
    { sHandle   :: Handle -- Handle of the script file.
    , sCmdPath  :: CmdPath -- Path of the solver.
    , sArgs     :: Args -- Args of the solver.
    , sFilePath :: FilePath -- File path of the script.
    }

--Writes to the script the command given.
writeToScript :: ScriptConf -> Command -> IO ()
writeToScript sConf cmd = do
  let scmd = render (pp  cmd) ++ "\n"
  hPutStr (sHandle sConf) scmd
  hFlush (sHandle sConf)


--Function that only writes to the script a command and returns an empty string
scriptFun :: ScriptConf -> Command -> IO String
scriptFun sConf cmd = writeToScript sConf cmd >> return ""

--Function that writes to the script a comaand, executes the solver and reads
--the response.
scriptFunExec :: ScriptConf -> Command -> IO String
scriptFunExec sConf cmd = do
  writeToScript sConf cmd
  res <- sendScript (sCmdPath sConf) (sArgs sConf) (sFilePath sConf)
  return $ last $ lines res



scriptGenResponse :: ScriptConf -> Command -> IO GenResult
scriptGenResponse sConf cmd = writeToScript sConf cmd >> return Success


scriptCheckSatResponse :: ScriptConf -> Command -> IO SatResult
scriptCheckSatResponse conf cmd =
  liftA checkSatResponse  (scriptFunExec conf cmd)

scriptGetValueResponse :: ScriptConf  -> Command -> IO GValResult
scriptGetValueResponse conf cmd =
  liftA getValueResponse (scriptFunExec conf cmd)


--SMT Commands.

-- scriptCheckSat, scriptGetAssertins, scriptGetValue, scriptGetValue,
-- scriptGetProof, scriptGetUnsatCore, scriptGetInfo, scriptGetOption,
-- script Exit.
--All above functions use ScriptFunExc the rest use scriptFun.


scriptSetLogic :: ScriptConf -> Name -> IO GenResult
scriptSetLogic sConf name = scriptGenResponse sConf (CmdSetLogic name )

scriptSetOption :: ScriptConf -> Option -> IO GenResult
scriptSetOption sConf option = scriptGenResponse sConf (CmdSetOption option)

scriptSetInfo :: ScriptConf -> Attr -> IO GenResult
scriptSetInfo sConf attr  = scriptGenResponse sConf  (CmdSetInfo attr)

scriptDeclareType :: ScriptConf -> Name -> Integer -> IO GenResult
scriptDeclareType sConf name number =
    scriptGenResponse sConf (CmdDeclareType name number)

scriptDefineType :: ScriptConf  -> Name -> [Name] -> Type -> IO GenResult
scriptDefineType sConf name names t =
    scriptGenResponse sConf (CmdDefineType name names t)

scriptDeclareFun :: ScriptConf  -> Name -> [Type] -> Type -> IO GenResult
scriptDeclareFun sConf name lt t =
    scriptGenResponse sConf (CmdDeclareFun name lt t)

scriptDefineFun :: ScriptConf -> Name -> [Binder] -> Type -> Expr -> IO GenResult
scriptDefineFun sConf name binders t expression =
    scriptGenResponse sConf (CmdDefineFun name binders t expression)

scriptPush :: ScriptConf -> Integer -> IO GenResult
scriptPush sConf number = scriptGenResponse sConf (CmdPush number)

scriptPop :: ScriptConf -> Integer -> IO GenResult
scriptPop sConf number =
 scriptGenResponse sConf (CmdPop number)

scriptAssert :: ScriptConf -> Expr -> IO GenResult
scriptAssert sConf expression =
    scriptGenResponse sConf (CmdAssert expression)

scriptCheckSat :: ScriptConf -> IO SatResult
scriptCheckSat sConf = scriptCheckSatResponse sConf CmdCheckSat

scriptGetAssertions :: ScriptConf -> IO String
scriptGetAssertions sConf = scriptFunExec sConf  CmdGetAssertions

scriptGetValue :: ScriptConf -> [Expr] -> IO GValResult
scriptGetValue sConf exprs = scriptGetValueResponse sConf ( CmdGetValue exprs)

scriptGetProof :: ScriptConf -> IO String
scriptGetProof sConf  = scriptFunExec sConf  CmdGetProof

scriptGetUnsatCore :: ScriptConf -> IO String
scriptGetUnsatCore sConf = scriptFunExec sConf CmdGetUnsatCore

scriptGetInfo :: ScriptConf-> InfoFlag -> IO String
scriptGetInfo sConf info = scriptFunExec sConf ( CmdGetInfo info )

scriptGetOption :: ScriptConf -> Name -> IO String
scriptGetOption sConf name = scriptFunExec sConf ( CmdGetOption name )

scriptExit :: ScriptConf -> IO String
scriptExit sConf = do
  result <- scriptFunExec sConf CmdExit --Write to the script and execute
  hClose (sHandle sConf) -- close the handle of the script
  return result
