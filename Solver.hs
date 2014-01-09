module Solver(startSmt, stdInIsOpen) where

import System.Process
import GHC.IO.Handle
import Data.Maybe

type SmtPath = String
type Args = [String] 
type Smt = IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

newProcess :: SmtPath -> Args -> CreateProcess
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


startSmt :: SmtPath -> Args -> Maybe Smt
startSmt [] _ = Nothing
startSmt p a  = Just (createProcess$newProcess p a)


ymap :: (Handle -> Bool) -> Maybe Handle -> IO Bool
ymap _ Nothing = return False
ymap f (Just x) = return $ f x


cenas :: Maybe Handle -> IO Bool
cenas Nothing = return False
cenas (Just x) = hIsOpen x

stdInIsOpen :: Maybe Smt -> IO Bool
stdInIsOpen Nothing = return False
stdInIsOpen (Just smt) = do 
                        (a,b,c,d) <- smt
                        cenas a

escreve :: Maybe Handle -> String -> IO ()
escreve Nothing _ =  return ()
escreve (Just x) s = hPutStr x s



le':: Maybe Handle-> IO ()
le' Nothing = return ()
le' (Just x) = hGetContents x >>= print

le :: Maybe Handle  -> IO String
le Nothing = return "Nada"
le (Just x) = hGetContents x


write :: Maybe Smt -> String -> IO String
write  Nothing _ = return "Nada"
write (Just x)  s = do
                (a,b,c,d) <- x
                escreve a s
                le b
                
le'' :: Maybe Smt  -> IO ()
le'' Nothing = return ()
le'' (Just x) = do
              (a,b,c,d) <- x
              le' b
