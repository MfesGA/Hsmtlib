module Solver
    ( beginProcess
    , send
    , endProcess
    ) where

import System.Process
import GHC.IO.Handle
import Data.Maybe
import Control.Exception
import System.IO.Error

-- | Path to the process
type CmdPath = String

-- | Argumants passed to process
type Args = [String] 

-- |Type returned by CreateProcess
type Process = 
    ( Maybe Handle -- process std_in pipe
    , Maybe Handle -- process std_out pipe
    , Maybe Handle -- process std_err pipe
    , ProcessHandle -- process pid
    )

{- |
    Generates a CreateProcess 
    with just the command,
    the arguments
    and creates the pipes to comunicate
-}
newProcess :: CmdPath -> Args -> CreateProcess
newProcess p a = CreateProcess
    { cmdspec = RawCommand p a
    , cwd = Nothing
    , env = Nothing
    , std_in = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    , close_fds = False
    , create_group =  False 
	  }
	  


-- | Creates a Process ready to be executed.
beginProcess :: CmdPath -> Args -> (IO Process)
beginProcess cmd path  = createProcess (newProcess cmd path)


-- | trys to run the function. 
tryIO ::(a -> IO b ) -> a -> IO(Either IOException b)
tryIO f arg = try $ f arg

       
{- |
    Sends the desired input to the process std_in and then reads from std out.
    Working smt with this method:
      - z3
      - mathSat
      - cvc4 gives the exception(Broken pipe) when trying to read from std out
-}
send :: Process -> String -> IO String
send (Just std_in, Just std_out,_,_) cmd =  do
    let put_str = (flip hPutStr) cmd
    resPut <-tryIO put_str std_in -- trys to write to std in 
    case resPut of
      --If there was an excepion writing then return the error 
      Left exception -> return $ "send1: " ++ show exception
      Right _ -> do  -- if it was successeful  
        resFlush <- tryIO hFlush std_in -- trys to flush std in     
        case resFlush of
          --if there was an exception flushing then return the error 
          Left exception -> return $ "send2: "  ++ show exception
          --if it was succeful then start reading from the std out
          Right _ -> readResponse (-1) ""  std_out

{- |
    Receive a inital time to wait for the process to write to the handle,
    a String wich will be added the text read from the handle and the handle.
    If it was able to read a line from the handle then call  the function again
    but with time equals to 10.
    Working smt with this methid:
      - z3 
      - mathSat
      - cvc4 gives the exception(Broken pipe) when trying to read from std out
-}
readResponse :: Int -> String -> Handle -> IO String
readResponse time str handle = do
  -- if the process dosent write to std out this function will block.
  let hWait =(flip hWaitForInput) time
  read <- tryIO hWait handle -- trys to wait for some output in std out.
  case read of
    -- if the wait gave an exception returns the error.
    Left exception -> return $ "readResponse1:" ++ (show exception)
    Right False -> return str  -- returns the lines read until now.
    Right True -> do
      -- if there is something to read then trys to read a line.
      res_get <- tryIO hGetLine handle      
      case res_get of
        -- if there was an exception then return it.
        Left exception -> return $ "readResponse2:" ++ (show exception)
        --  if some text was read then trys to read the pipe again.  
        Right text -> readResponse 10 (str++text) handle

   
-- | Sends the signal to terminate to the running process.
endProcess :: Process -> IO()
endProcess (_,_,_,processHandle) = do
  terminateProcess processHandle
  waitForProcess processHandle >>= print
  

-- z3 works fine.
z3 :: IO()
z3 = do
  smt <- beginProcess "z3" ["-smt2","-in"]
  send smt "(set-option :print-success true)\n" >>= print
  send smt "(declare-const a Int)\n" >>= print
  send smt "(declare-fun f (Int Bool) Int)\n" >>= print  
  send smt "(assert (> a 10))\n" >>= print
  send smt "(assert (< (f a true) 100))\n" >>= print
  send smt "(check-sat)\n" >>= print
  send smt "(get-model)\n" >>= print
  endProcess smt
  
 
-- sonolar inst working.
sonolar :: IO ()
sonolar = do
  smt <- beginProcess "sonolar" ["--print-model"]
  send smt "(set-option :print-success true)\n" >>= print
  send smt "(declare-const a Int)\n" >>= print
  send smt "(declare-fun f (Int Bool) Int)\n" >>= print  
  send smt "(assert (> a 10))\n" >>= print
  send smt "(assert (< (f a true) 100))\n" >>= print
  send smt "(check-sat)\n" >>= print
  send smt "(get-model)\n" >>= print
  endProcess smt
  

-- mathSat works fine.
mathSat :: IO()
mathSat = do
  smt <- beginProcess "mathsat" []
  send smt "(set-option :print-success true)\n" >>= print
  send smt "(declare-const a Int)\n" >>= print
  send smt "(declare-fun f (Int Bool) Int)\n" >>= print  
  send smt "(assert (> a 10))\n" >>= print
  send smt "(assert (< (f a true) 100))\n" >>= print
  send smt "(check-sat)\n" >>= print
  send smt "(get-model)\n" >>= print
  endProcess smt
  
-- cvc dosen't work, breaks the pipe and prints to std out.   
cvc4 :: IO()
cvc4 = do
  smt <- beginProcess "cvc4" ["--smtlib-strict","--print-succes"]
  send smt "(set-option :print-success true)\n" >>= print
  --CVC4 wont accept the next command and print the warning to std_err 
  send smt "(declare-const a Int)\n" >>= print 
  send smt "(declare-fun f (Int Bool) Int)\n" >>= print  
  send smt "(assert (> a 10))\n" >>= print
  send smt "(assert (< (f a true) 100))\n" >>= print
  --send smt "(check-sat)\n" >>= print
  --send smt "(get-model)\n" >>= print
  --send smt "(declare-const a Int)\n" >>= print 
  --send smt "(declare-fun f (Int Bool) Int)\n" >>= print  
  --send smt "(assert (> a 10))\n" >>= print
  --send smt "(assert (< (f a true) 100))\n" >>= print
  --send smt "(check-sat)\n" >>= print
  --send smt "(get-model)\n" >>= print
  endProcess smt  
