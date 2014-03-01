module ScriptCmd where

import Process
import SMTLib2
import Solver(Result)
import Text.PrettyPrint
import System.IO(hPutStr,hFlush,Handle,hClose)

{-|
    Module where all the commands are defined with the script method from Process.hs
 -}

data Srcmd = Srcmd
    { handle :: Handle
    , cmdPath  :: CmdPath
    , args :: Args
    , filePath :: FilePath
    }

scriptFun ::  Srcmd-> Command -> IO Result
scriptFun srcmd cmd = do
  let scmd = render ( pp  cmd ) ++ "\n" 
  hPutStr ( handle srcmd ) scmd
  hFlush ( handle srcmd )
  sendScript ( cmdPath srcmd ) ( args srcmd )  ( filePath srcmd )

scriptSetLogic :: Srcmd -> Name -> IO Result
scriptSetLogic srcmd name = scriptFun srcmd ( CmdSetLogic name )

scriptSetOption :: Srcmd -> Option -> IO Result
scriptSetOption srcmd option = scriptFun srcmd ( CmdSetOption option)

scriptSetInfo :: Srcmd -> Attr -> IO Result
scriptSetInfo srcmd attr  = scriptFun srcmd  (CmdSetInfo attr) 

scriptDeclareType :: Srcmd -> Name -> Integer -> IO Result
scriptDeclareType srcmd name int = scriptFun srcmd ( CmdDeclareType name int)

scriptDefineType :: Srcmd  -> Name -> [Name] -> Type -> IO Result
scriptDefineType srcmd name names t = scriptFun srcmd ( CmdDefineType name names t )

scriptDeclareFun :: Srcmd  -> Name -> [Type] -> Type -> IO Result
scriptDeclareFun srcmd name lt t = scriptFun srcmd ( CmdDeclareFun name lt t )

scriptDefineFun :: Srcmd -> Name -> [Binder] -> Type -> Expr -> IO Result
scriptDefineFun srcmd name binders t exp = scriptFun srcmd ( CmdDefineFun name binders t exp)

scriptPush :: Srcmd -> Integer -> IO Result
scriptPush srcmd int = scriptFun srcmd ( CmdPush int )

scriptPop :: Srcmd -> Integer -> IO Result
scriptPop srcmd int = scriptFun srcmd ( CmdPop int )

scriptAssert :: Srcmd -> Expr -> IO Result
scriptAssert srcmd exp = scriptFun srcmd ( CmdAssert exp)

scriptCheckSat :: Srcmd -> IO Result
scriptCheckSat srcmd = scriptFun srcmd CmdCheckSat

scriptGetAssertions :: Srcmd -> IO Result
scriptGetAssertions srcmd = scriptFun srcmd  CmdGetAssertions 

scriptGetValue :: Srcmd -> [Expr] -> IO Result
scriptGetValue srcmd exprs = scriptFun srcmd ( CmdGetValue exprs)

scriptGetProof :: Srcmd -> IO Result
scriptGetProof srcmd  = scriptFun srcmd  CmdGetProof

scriptGetUnsatCore :: Srcmd -> IO Result
scriptGetUnsatCore srcmd = scriptFun srcmd CmdGetUnsatCore 

scriptGetInfo :: Srcmd-> InfoFlag -> IO Result
scriptGetInfo srcmd info = scriptFun srcmd ( CmdGetInfo info )

scriptGetOption :: Srcmd -> Name -> IO Result
scriptGetOption srcmd name = scriptFun srcmd ( CmdGetOption name )

scriptExit :: Handle -> Srcmd -> IO Result
scriptExit handle srcmd = do
  result <- scriptFun srcmd CmdExit 
  hClose handle
  return result