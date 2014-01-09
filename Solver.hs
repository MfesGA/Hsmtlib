module Solver(beginProcess, execute,endProcess) where

import System.Process
import GHC.IO.Handle
import Data.Maybe


type CmdPath = String
type Args = [String] 
type Process = (Maybe Handle,
                     Maybe Handle,
                     Maybe Handle,
                     ProcessHandle)


newProcess :: CmdPath -> Args -> CreateProcess
newProcess p a = CreateProcess { 
        cmdspec = cmd,
				cwd = Nothing, 
				env = Nothing, 
				std_in = CreatePipe,
				std_out = CreatePipe,
				std_err = Inherit,{-If the processs has an error it 
				                    prints to the stderr of the 
				                    process running this file std-}
				close_fds = False, 
				create_group =  False 
				}where cmd = RawCommand p a


beginProcess :: CmdPath -> Args -> (IO Process)
beginProcess cmd path  = createProcess (newProcess cmd path)




execute :: Process -> String -> IO String
execute (Just std_in, Just std_out,_,_) cmd = do
  print "ola"
  hPutStr std_in cmd 
  hFlush std_in
  print "cenas"  
  b<-(hGetLine std_out)  
  print "xau"   
  return b

endProcess :: Process -> IO()
endProcess (_,_,_,processHandle) = do
                 terminateProcess processHandle
                 waitForProcess processHandle >>= print
                         
                         
main :: IO()
main = do 
  a <- beginProcess "z3" ["-smt2","-in"]
  --hGetContents b >>= print
  --status a
  execute a "(set-option :print-success true)" >>= print
  --status a
  --execute a "(declare-fun f (Int Bool) Int)" >>= print
  --execute a "(declare-fun f (Int Bool) Int)" >>= print  
 -- execute a "(echo \"before reset\")" >>= print
  --status a
  --endProcess a
