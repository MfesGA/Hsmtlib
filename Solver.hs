module Solver(beginProcess, beginProcess',execute', execute,endProcess, status) where

import Control.Concurrent
import System.Process
import GHC.IO.Handle
import Data.Maybe

type CmdPath = String
type Args = [String] 
type Process = IO (Maybe Handle,
                     Maybe Handle,
                     Maybe Handle,
                     ProcessHandle)


newProcess :: CmdPath -> Args -> CreateProcess
newProcess p a = CreateProcess { 
        cmdspec = cmd,
				cwd = Nothing, 
				env = Nothing, 
				std_in = CreatePipe,--Ver se estes createPipe estao certas 
				std_out = CreatePipe,
				std_err = CreatePipe,
				close_fds = True, -- ver se funciona no windows 
				create_group =  True --cria um novo grupo de processos ?
				}where cmd = RawCommand p a


beginProcess :: CmdPath -> Args -> Maybe Process
beginProcess [] _ = Nothing
beginProcess p a  = Just(createProcess$newProcess p a)

beginProcess' :: CmdPath -> Args ->Maybe (IO( Handle, Handle, Handle, ProcessHandle))
beginProcess' [] _ = Nothing
beginProcess' p a = Just( runInteractiveProcess p a Nothing Nothing)

write :: Maybe Handle -> String -> IO ()
write Nothing _ = return ()
write (Just handle) cmd = hPutStr handle cmd 

result :: Maybe Handle -> IO String
result Nothing = return "Erro ao ler"
result (Just handle) = hGetLine handle

execute :: Maybe Process -> String -> IO String
execute Nothing _ = return "O Smt não esta inicializado"
execute (Just smt) cmd = do
               (stdin,stdout,stderr,_) <- smt    
               hSetBuffering (getX stdout) LineBuffering            
               write stdin cmd
               hWaitForInput (getX stdout) (-1)                
               result stdout            


execute' :: Maybe (IO(Handle, Handle, Handle, ProcessHandle)) -> String -> IO()
execute' Nothing _ = return ()
execute' (Just x) cmd = do
          (stdin,stdout,_,_) <- x
          forkIO $ hPutStr stdin cmd
          hGetContents stdout >>= print
         
getX :: Maybe a -> a
getX (Just x) =  x

status :: Maybe Process -> IO ()
status Nothing = return ()
status (Just x) = do
                 (_,stdinx,_,_) <- x
                 let stdin = getX stdinx   
                 hIsEOF stdin >>= print
                 hGetBuffering stdin >>= print
                 hIsOpen stdin >>= print

endProcess :: Maybe Process -> IO()
endProcess Nothing = return ()
endProcess (Just smt) = do
                         (_,_,_,processHandle) <- smt
                         terminateProcess processHandle
                         waitForProcess processHandle >>= print
                         
                         
main :: IO()
main = do 
  let a = beginProcess "z3" ["-smt2 -in"]
  status a
  execute a "(set-option :print-success true)" >>= print
  status a
  execute a "(set-option :print-success true)" >>= print  
  execute a "(declare-const x Int)" >>= print
  status a
  endProcess a
