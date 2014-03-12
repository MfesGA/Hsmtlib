module ScriptCmd where

import           Process
import           SMTLib2
import           Solver           (Result)
import           System.IO        (Handle, hClose, hFlush, hPutStr)
import           Text.PrettyPrint

{-|
    Module where all the commands are defined with the script method from Process.hs
 -}

data ScriptArgs = ScriptArgs
    { handle   :: Handle
    , cmdPath  :: CmdPath
    , args     :: Args
    , filePath :: FilePath
    }

scriptFun ::  ScriptArgs-> Command -> IO Result
scriptFun scriptArgs cmd = do
  let scmd = render ( pp  cmd ) ++ "\n"
  hPutStr ( handle scriptArgs ) scmd
  hFlush ( handle scriptArgs )
  sendScript ( cmdPath scriptArgs ) ( args scriptArgs )  ( filePath scriptArgs )

scriptSetLogic :: ScriptArgs -> Name -> IO Result
scriptSetLogic scriptArgs name = scriptFun scriptArgs ( CmdSetLogic name )

scriptSetOption :: ScriptArgs -> Option -> IO Result
scriptSetOption scriptArgs option = scriptFun scriptArgs ( CmdSetOption option)

scriptSetInfo :: ScriptArgs -> Attr -> IO Result
scriptSetInfo scriptArgs attr  = scriptFun scriptArgs  (CmdSetInfo attr)

scriptDeclareType :: ScriptArgs -> Name -> Integer -> IO Result
scriptDeclareType scriptArgs name number = scriptFun scriptArgs ( CmdDeclareType name number)

scriptDefineType :: ScriptArgs  -> Name -> [Name] -> Type -> IO Result
scriptDefineType scriptArgs name names t = scriptFun scriptArgs ( CmdDefineType name names t )

scriptDeclareFun :: ScriptArgs  -> Name -> [Type] -> Type -> IO Result
scriptDeclareFun scriptArgs name lt t = scriptFun scriptArgs ( CmdDeclareFun name lt t )

scriptDefineFun :: ScriptArgs -> Name -> [Binder] -> Type -> Expr -> IO Result
scriptDefineFun scriptArgs name binders t expression = scriptFun scriptArgs ( CmdDefineFun name binders t expression)

scriptPush :: ScriptArgs -> Integer -> IO Result
scriptPush scriptArgs number = scriptFun scriptArgs ( CmdPush number )

scriptPop :: ScriptArgs -> Integer -> IO Result
scriptPop scriptArgs number = scriptFun scriptArgs ( CmdPop number )

scriptAssert :: ScriptArgs -> Expr -> IO Result
scriptAssert scriptArgs expression = scriptFun scriptArgs ( CmdAssert expression)

scriptCheckSat :: ScriptArgs -> IO Result
scriptCheckSat scriptArgs = scriptFun scriptArgs CmdCheckSat

scriptGetAssertions :: ScriptArgs -> IO Result
scriptGetAssertions scriptArgs = scriptFun scriptArgs  CmdGetAssertions

scriptGetValue :: ScriptArgs -> [Expr] -> IO Result
scriptGetValue scriptArgs exprs = scriptFun scriptArgs ( CmdGetValue exprs)

scriptGetProof :: ScriptArgs -> IO Result
scriptGetProof scriptArgs  = scriptFun scriptArgs  CmdGetProof

scriptGetUnsatCore :: ScriptArgs -> IO Result
scriptGetUnsatCore scriptArgs = scriptFun scriptArgs CmdGetUnsatCore

scriptGetInfo :: ScriptArgs-> InfoFlag -> IO Result
scriptGetInfo scriptArgs info = scriptFun scriptArgs ( CmdGetInfo info )

scriptGetOption :: ScriptArgs -> Name -> IO Result
scriptGetOption scriptArgs name = scriptFun scriptArgs ( CmdGetOption name )

scriptExit :: Handle -> ScriptArgs -> IO Result
scriptExit scriptHandle scriptArgs = do
  result <- scriptFun scriptArgs CmdExit
  hClose scriptHandle
  return result
