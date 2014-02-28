module ScriptCmd where

import Process
import SMTLib2
import Solver(Result)
import Text.PrettyPrint
import System.IO(hPutStr,hFlush,Handle)

{-|
    Module where all the commands are defined with the script method from Process.hs
 -}

data Srcmd = Srcmd
    { handle :: Handle
    , cmdPath  :: CmdPath
    , args :: Args
    , filePath :: FilePath
    }

scriptFun ::  Process -> Srcmd-> Command -> IO Result
scriptFun proc srcmd cmd = do
  let scmd = render ( pp  cmd ) ++ "\n" 
  hPutStr ( handle srcmd ) scmd
  hFlush ( handle srcmd )
  sendScript ( cmdPath srcmd ) ( args srcmd )  ( filePath srcmd )

scriptSetLogic :: Process -> Srcmd -> Name -> IO Result
scriptSetLogic proc srcmd name = scriptFun proc srcmd ( CmdSetLogic name )

scriptSetOption :: Process ->  Srcmd -> Option -> IO Result
scriptSetOption proc srcmd option = scriptFun proc srcmd ( CmdSetOption option)

scriptSetInfo :: Process -> Srcmd -> Attr -> IO Result
scriptSetInfo proc srcmd attr  = scriptFun proc srcmd  (CmdSetInfo attr) 

scriptDeclareType :: Process -> Srcmd -> Name -> Integer -> IO Result
scriptDeclareType proc srcmd name int = scriptFun proc srcmd ( CmdDeclareType name int)

scriptDefineType :: Process -> Srcmd  -> Name -> [Name] -> Type -> IO Result
scriptDefineType proc srcmd name names t = scriptFun proc  srcmd ( CmdDefineType name names t )

scriptDeclareFun :: Process -> Srcmd  -> Name -> [Type] -> Type -> IO Result
scriptDeclareFun proc srcmd name lt t = scriptFun  proc srcmd ( CmdDeclareFun name lt t )

scriptDefineFun :: Process -> Srcmd -> Name -> [Binder] -> Type -> Expr -> IO Result
scriptDefineFun proc srcmd name binders t exp = scriptFun proc srcmd ( CmdDefineFun name binders t exp)

scriptPush :: Process -> Srcmd -> Integer -> IO Result
scriptPush proc  srcmd int = scriptFun proc srcmd ( CmdPush int )

scriptPop :: Process ->  Srcmd -> Integer -> IO Result
scriptPop proc srcmd int = scriptFun proc srcmd ( CmdPop int )

scriptAssert :: Process -> Srcmd -> Expr -> IO Result
scriptAssert proc srcmd exp = scriptFun proc srcmd ( CmdAssert exp)

scriptCheckSat :: Process -> Srcmd -> IO Result
scriptCheckSat proc srcmd = scriptFun proc srcmd CmdCheckSat

scriptGetAssertions :: Process -> Srcmd -> IO Result
scriptGetAssertions proc srcmd = scriptFun proc srcmd  CmdGetAssertions 

scriptGetValue :: Process -> Srcmd -> [Expr] -> IO Result
scriptGetValue proc srcmd exprs = scriptFun proc srcmd ( CmdGetValue exprs)

scriptGetProof :: Process -> Srcmd -> IO Result
scriptGetProof proc srcmd  = scriptFun proc srcmd  CmdGetProof

scriptGetUnsatCore :: Process -> Srcmd -> IO Result
scriptGetUnsatCore proc srcmd = scriptFun proc srcmd CmdGetUnsatCore 

scriptGetInfo :: Process -> Srcmd-> InfoFlag -> IO Result
scriptGetInfo proc srcmd info = scriptFun proc srcmd ( CmdGetInfo info )

scriptGetOption :: Process -> Srcmd -> Name -> IO Result
scriptGetOption proc srcmd name = scriptFun proc srcmd ( CmdGetOption name )

scriptExit :: Process -> Srcmd -> IO Result
scriptExit proc srcmd = scriptFun proc srcmd CmdExit 
