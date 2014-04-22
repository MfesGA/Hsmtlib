{- |
Module      : OnlineCmd

Module with the functions used in online Mode.
-}

module Hsmtlib.Solvers.Cmd.OnlineCmd where

import           Hsmtlib.Solver
import           Hsmtlib.Solvers.Cmd.ProcCom.Process
import           SMTLib2
import           Text.PrettyPrint
import           Hsmtlib.Solvers.Cmd.Parser.CmdResult
import           Control.Applicative(liftA)

--Uses the function  send from Cmd.Solver to send the command.
onlineFun ::  Process  -> Command -> IO String
onlineFun proc cmd = send proc (render (pp  cmd) ++ "\n")

onlineGenResponse :: Process -> Command -> IO Result
onlineGenResponse proc cmd  = liftA genResponse (onlineFun proc cmd)

onlineCheckSatResponse :: Process -> Command -> IO Result
onlineCheckSatResponse proc cmd = liftA checkSatResponse (onlineFun proc cmd)



--SMT Commands.

onlineSetLogic :: Process -> Name -> IO Result
onlineSetLogic proc name = onlineGenResponse proc (CmdSetLogic name)

onlineSetOption :: Process -> Option -> IO Result
onlineSetOption proc option = onlineGenResponse proc (CmdSetOption option)

onlineSetInfo :: Process ->  Attr -> IO Result
onlineSetInfo proc attr  = onlineGenResponse proc  (CmdSetInfo attr)

onlineDeclareType :: Process -> Name -> Integer -> IO Result
onlineDeclareType proc name number =
    onlineGenResponse proc (CmdDeclareType name number)

onlineDefineType :: Process -> Name -> [Name] -> Type -> IO Result
onlineDefineType proc name names t =
    onlineGenResponse proc (CmdDefineType name names t)

onlineDeclareFun :: Process -> Name -> [Type] -> Type -> IO Result
onlineDeclareFun proc name lt t =
    onlineGenResponse proc (CmdDeclareFun name lt t)

onlineDefineFun :: Process -> Name -> [Binder] -> Type -> Expr -> IO Result
onlineDefineFun proc name binders t expression =
    onlineGenResponse proc (CmdDefineFun name binders t expression)

onlinePush :: Process -> Integer -> IO Result
onlinePush proc number = onlineGenResponse proc (CmdPush number)

onlinePop :: Process -> Integer -> IO Result
onlinePop proc number = onlineGenResponse proc (CmdPop number)

onlineAssert :: Process -> Expr -> IO Result
onlineAssert proc expression = onlineGenResponse proc (CmdAssert expression)

onlineCheckSat :: Process  -> IO Result
onlineCheckSat proc = onlineCheckSatResponse proc CmdCheckSat

onlineGetAssertions :: Process -> IO String
onlineGetAssertions proc = onlineFun proc  CmdGetAssertions

onlineGetValue :: Process -> [Expr] -> IO String
onlineGetValue proc exprs = onlineFun proc ( CmdGetValue exprs)

onlineGetProof :: Process -> IO String
onlineGetProof proc = onlineFun proc  CmdGetProof

onlineGetUnsatCore :: Process -> IO String
onlineGetUnsatCore proc = onlineFun proc CmdGetUnsatCore

onlineGetInfo :: Process -> InfoFlag -> IO String
onlineGetInfo proc info = onlineFun proc ( CmdGetInfo info )

onlineGetOption :: Process -> Name -> IO String
onlineGetOption proc name = onlineFun proc ( CmdGetOption name )

onlineExit :: Process -> IO String
onlineExit proc = onlineFun proc CmdExit
