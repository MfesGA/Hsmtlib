 {- |
Module      : Process

  The following module contains method that facilitate the comunication with
  the SMTSolvers.
-}
module Hsmtlib.Solvers.Cmd.ProcCom.Process
    ( beginProcess
    , send
    , endProcess
    , sendContext
    , sendScript
    , sendCvc4
    , Process
    , CmdPath
    , Args
    ) where

import           Control.Exception
import           GHC.IO.Handle
import           System.Exit
import           System.Process
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

-- | Context is just a string wich will be sent to std_in
type Context = String


-- Functions used in Online Mode


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
    , delegate_ctlc= True
    }

-- | Creates a Process ready to be executed.
beginProcess :: CmdPath -> Args -> IO Process
beginProcess cmd path  = createProcess (newProcess cmd path)
{--

beginProcess :: CmdPath -> Args -> IO Process
beginProcess cmdPath args = runInteractiveProcess cmdPath args Nothing Nothing
--}
-- | trys to run the function.
tryIO ::(a -> IO b ) -> a -> IO ( Either IOException b )
tryIO f arg = try $ f arg


{-
   #########################################################################
   #                                                                       #
   #         Functions to send command and read from  cvc4                 #
   #                                                                       #
   #########################################################################
-}



{-|
    Sends the desired input to the process std_in and then reads from std out.
    Working smt with this method:
   -Cvc4
-}
sendCvc4 :: Process -> String -> IO String
sendCvc4 (Just hIn, Just hOut, _, _) cmd =  do
    let put_str = flip hPutStr  cmd
    resPut <-tryIO put_str hIn -- trys to write to std in
    case resPut of
      --If there was an excepion writing then return the error
      Left exception -> return $ "send1: " ++ show exception
      Right _ -> do  -- if it was successeful
        resFlush <- tryIO hFlush hIn -- trys to flush std in
        case resFlush of
          --if there was an exception flushing then return the error
          Left exception -> return $ "send2: "  ++ show exception
          --if it was succeful then start reading from the std out
          Right _ ->  do
            -- the first line in the buffer is the command written 
            -- so we throw it away.
            res<- hGetLine hOut
            let b = reverse $ drop 1 $reverse cmd -- drop new line in linux
            case res == b of 
              True -> readResponseCvc4 (-1)  "" hOut -- linux
              False -> readResponseCvc4 (10)  res hOut -- windows

            
{-|
    Receive a inital time to wait for the process to write to the handle,
    a String wich will be added the text read from the handle and the handle.
    If it was able to read a line from the handle then call  the function again
    but with time equals to 10.
    Working smt with this methid:
    -Cvc4
-}
readResponseCvc4 :: Int -> String -> Handle -> IO String
readResponseCvc4 time str procHandle = do
  -- if the process dosent write to std out this function will block.
  let hWait = flip hWaitForInput time
  readOut <- tryIO hWait procHandle -- trys to wait for some output in std out.
  case readOut of
    -- if the wait gave an exception returns the error.
    Left exception -> return $ "readResponse1:" ++ show exception
    Right False -> return str  -- returns the lines read until now.
    Right True -> do
      -- if there is something to read then trys to read a line.
      res_get <- tryIO hGetLine procHandle
      case res_get of
        -- if there was an exception then return it.
        Left exception -> return $ "readResponse2:" ++ show exception
        --  if some text was read then trys to read the pipe again.
        Right text -> readResponse 10 (str++text) procHandle






{-
   #########################################################################
   #                                                                       #
   #         Functions to send command and read from most solvers          #
   #                                                                       #
   #########################################################################
-}



{-|
    Sends the desired input to the process std_in and then reads from std out.
    Working smt with this method:
      - z3
      - mathSat
-}
send :: Process -> String -> IO String
send (Just hIn, Just hOut, _, _) cmd =  do
    let put_str = flip hPutStr  cmd
    resPut <-tryIO put_str hIn -- trys to write to std in
    case resPut of
      --If there was an excepion writing then return the error
      Left exception -> return $ "send1: " ++ show exception
      Right _ -> do  -- if it was successeful
        resFlush <- tryIO hFlush hIn -- trys to flush std in
        case resFlush of
          --if there was an exception flushing then return the error
          Left exception -> return $ "send2: "  ++ show exception
          --if it was succeful then start reading from the std out
          Right _ -> readResponse (-1)  "" hOut

{-|
    Receive a inital time to wait for the process to write to the handle,
    a String wich will be added the text read from the handle and the handle.
    If it was able to read a line from the handle then call  the function again
    but with time equals to 10.
    Working smt with this methid:
      - z3
      - mathSat
-}
readResponse :: Int -> String -> Handle -> IO String
readResponse time str procHandle = do
  -- if the process dosent write to std out this function will block.
  let hWait = flip hWaitForInput time
  readOut <- tryIO hWait procHandle -- trys to wait for some output in std out.
  case readOut of
    -- if the wait gave an exception returns the error.
    Left exception -> return $ "readResponse1:" ++ show exception
    Right False -> return str  -- returns the lines read until now.
    Right True -> do
      -- if there is something to read then trys to read a line.
      res_get <- tryIO hGetLine procHandle
      case res_get of
        -- if there was an exception then return it.
        Left exception -> return $ "readResponse2:" ++ show exception
        --  if some text was read then trys to read the pipe again.
        Right text -> readResponse 10 (str++text) procHandle






-- | Sends the signal to terminate to the running process.
endProcess :: Process -> IO ExitCode
endProcess (_,_,_,processHandle) = do
  terminateProcess processHandle
  waitForProcess processHandle



-- Function used in ContextMode

{-|
  It's the same function as readProcess.
<http://hackage.haskell.org/package/process-1.1.0.1/docs/System-Process.html>
 -}
sendContext :: CmdPath -> Args -> Context -> IO String
sendContext = readProcess



{-|
Calls readProcess and pass as arguments the arguments given plus the name of
the file with the context.
An empty String is passed to the std_in.

It's the same function as readProcess.
readProcess:
<http://hackage.haskell.org/package/process-1.1.0.1/docs/System-Process.html>
-}

tryIOError::  IO a -> IO (Either IOError a)
tryIOError = try

sendScript :: CmdPath -> Args -> FilePath -> IO String
sendScript cmdPath args script_name  = do
   res <- tryIOError$ readProcess cmdPath (args ++ [script_name]) ""
   case res of
      Left err -> return (show err)
      Right val -> return val
