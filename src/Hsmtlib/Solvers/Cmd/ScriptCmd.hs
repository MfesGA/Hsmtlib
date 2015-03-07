{- |
Module      : ScriptCmd

Module with the functions used in script Mode.
-}
module Hsmtlib.Solvers.Cmd.ScriptCmd where

import           Control.Applicative                 (liftA)
import           Smtlib.Syntax.Syntax              (GenResponse (Success))
import           Hsmtlib.Solver
import           Hsmtlib.Solvers.Cmd.CmdResult
import           Hsmtlib.Solvers.Cmd.ProcCom.Process
import           Smtlib.Syntax.Syntax
import           Smtlib.Syntax.ShowSL
import           System.IO                           (Handle, hClose, hFlush,
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
  let scmd = showSL cmd ++ "\n"
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
  return $ last $lines res



scriptGenResponse :: ScriptConf -> Command -> IO Result
scriptGenResponse sConf cmd = writeToScript sConf cmd >> return (CGR Success)


scriptCheckSatResponse :: ScriptConf -> Command -> IO Result
scriptCheckSatResponse conf cmd =
  liftA checkSatResponse  (scriptFunExec conf cmd)


scriptGetValueResponse :: ScriptConf  -> Command -> IO Result
scriptGetValueResponse conf cmd =
  liftA getValueResponse (scriptFunExec conf cmd)


scriptGetInfoResponse :: ScriptConf  -> Command -> IO Result
scriptGetInfoResponse conf cmd =
 liftA getInfoResponse (scriptFunExec conf cmd)


scriptGetAssertionResponse :: ScriptConf  -> Command -> IO Result
scriptGetAssertionResponse conf cmd =
    liftA getAssertionResponse (scriptFunExec conf cmd)

scriptGetProofResponse :: ScriptConf  -> Command -> IO Result
scriptGetProofResponse conf cmd =
  liftA getProofResponse (scriptFunExec conf cmd)


scriptGetUnsatCoreResponse :: ScriptConf  -> Command -> IO Result
scriptGetUnsatCoreResponse conf cmd =
    liftA getUnsatCoreResponse (scriptFunExec conf cmd)


scriptGetAssignmentResponse :: ScriptConf  -> Command -> IO Result
scriptGetAssignmentResponse conf cmd =
    liftA getAssignmentResponse (scriptFunExec conf cmd)


scriptGetOptionResponse :: ScriptConf  -> Command -> IO Result
scriptGetOptionResponse conf cmd =
  liftA getOptionResponse (scriptFunExec conf cmd)





--SMT Commands.

-- scriptCheckSat, scriptGetAssertins, scriptGetValue, scriptGetValue,
-- scriptGetProof, scriptGetUnsatCore, scriptGetInfo, scriptGetOption,
-- script Exit.
--All above functions use ScriptFunExc the rest use scriptFun.


scriptSetLogic :: ScriptConf -> String -> IO Result
scriptSetLogic sConf name = scriptGenResponse sConf (SetLogic name )

scriptSetOption :: ScriptConf -> Option -> IO Result
scriptSetOption sConf option = scriptGenResponse sConf (SetOption option)

scriptSetInfo :: ScriptConf -> Attribute -> IO Result
scriptSetInfo sConf attr  = scriptGenResponse sConf  (SetInfo attr)

scriptDeclareSort :: ScriptConf -> String -> Int -> IO Result
scriptDeclareSort sConf name number =
    scriptGenResponse sConf (DeclareSort name number)

scriptDefineSort :: ScriptConf  -> String -> [String] -> Sort -> IO Result
scriptDefineSort sConf name names t =
    scriptGenResponse sConf (DefineSort name names t)

scriptDeclareFun :: ScriptConf  -> String -> [Sort] -> Sort -> IO Result
scriptDeclareFun sConf name lt t =
    scriptGenResponse sConf (DeclareFun name lt t)

scriptDefineFun :: ScriptConf -> String -> [SortedVar] -> Sort -> Term -> IO Result
scriptDefineFun sConf name binders t expression =
    scriptGenResponse sConf (DefineFun name binders t expression)

scriptPush :: ScriptConf -> Int -> IO Result
scriptPush sConf number = scriptGenResponse sConf (Push number)

scriptPop :: ScriptConf -> Int -> IO Result
scriptPop sConf number =
 scriptGenResponse sConf (Pop number)

scriptAssert :: ScriptConf -> Term -> IO Result
scriptAssert sConf expression =
    scriptGenResponse sConf (Assert expression)

scriptCheckSat :: ScriptConf -> IO Result
scriptCheckSat sConf = scriptCheckSatResponse sConf CheckSat

scriptGetAssertions :: ScriptConf -> IO Result
scriptGetAssertions sConf = scriptGetAssertionResponse sConf GetAssertions

scriptGetValue :: ScriptConf -> [Term] -> IO Result
scriptGetValue sConf exprs = scriptGetValueResponse sConf (GetValue exprs)

scriptGetProof :: ScriptConf -> IO Result
scriptGetProof sConf  = scriptGetProofResponse sConf GetProof

scriptGetUnsatCore :: ScriptConf -> IO Result
scriptGetUnsatCore sConf = scriptGetUnsatCoreResponse sConf GetUnsatCore

scriptGetInfo :: ScriptConf-> InfoFlags -> IO Result
scriptGetInfo sConf info = scriptGetInfoResponse sConf (GetInfo info)

scriptGetOption :: ScriptConf -> String -> IO Result
scriptGetOption sConf name = scriptGetOptionResponse sConf (GetOption name)

scriptExit :: ScriptConf -> IO Result
scriptExit sConf = do
  _ <- scriptFun sConf Exit --Write to the script and execute
  hClose (sHandle sConf) -- close the handle of the script
  return (CGR Success)
