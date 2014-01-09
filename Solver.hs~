module Solver(beginProcess, execute,endProcess) where
-- let a = beginProcess "cvc4" ["--smtlib"]

import System.Process
import GHC.IO.Handle
import Data.Maybe

type ProcessPath = String
type Args = [String] 
type Process = IO (Maybe Handle,
                     Maybe Handle,
                     Maybe Handle,
                     ProcessHandle)


newProcess :: ProcessPath -> Args -> CreateProcess
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


beginProcess :: ProcessPath -> Args ->Maybe Process
beginProcess [] _ = Nothing
beginProcess p a  = Just (createProcess$newProcess p a)


write :: Maybe Handle -> String -> IO ()
write Nothing _ = return ()
write (Just handle) cmd = hPutStr handle cmd 

result :: Maybe Handle -> IO String
result Nothing = return "Erro ao ler"
result (Just handle) = hGetContents handle

execute :: Maybe Process -> String -> IO String
execute Nothing _ = return "O Smt não esta inicializado"
execute (Just smt) cmd = do
               (stdin,stdout,stderr,_) <- smt
               write stdin cmd
               result stderr               
               result stdout

endProcess :: Maybe Process -> IO()
endProcess Nothing = return ()
endProcess (Just smt) = do
                         (_,_,_,processHandle) <- smt
                         terminateProcess processHandle
                         waitForProcess processHandle >>= print
