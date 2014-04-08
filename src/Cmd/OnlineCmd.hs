{- |
Module      : OnlineCmd

Module with the functions used in online Mode.
-}

module Cmd.OnlineCmd where

import           Cmd.ProcCom.Process
import           SMTLib2
import           Cmd.Solver           (Result)
import           Text.PrettyPrint


--Uses the function  send from Cmd.Solver to send the command.
onlineFun ::  Process  -> Command -> IO Result
onlineFun proc cmd = send proc (render (pp  cmd) ++ "\n\n")


--SMT Commands.


onlineSetLogic :: Process -> Name -> IO Result
onlineSetLogic proc name = onlineFun proc ( CmdSetLogic name )

onlineSetOption :: Process -> Option -> IO Result
onlineSetOption proc option = onlineFun proc ( CmdSetOption option)

onlineSetInfo :: Process ->  Attr -> IO Result
onlineSetInfo proc attr  = onlineFun proc  (CmdSetInfo attr)

onlineDeclareType :: Process -> Name -> Integer -> IO Result
onlineDeclareType proc name number =
    onlineFun proc ( CmdDeclareType name number)

onlineDefineType :: Process -> Name -> [Name] -> Type -> IO Result
onlineDefineType proc name names t =
    onlineFun proc ( CmdDefineType name names t )

onlineDeclareFun :: Process -> Name -> [Type] -> Type -> IO Result
onlineDeclareFun proc name lt t = onlineFun  proc ( CmdDeclareFun name lt t )

onlineDefineFun :: Process -> Name -> [Binder] -> Type -> Expr -> IO Result
onlineDefineFun proc name binders t expression =
    onlineFun proc ( CmdDefineFun name binders t expression)

onlinePush :: Process -> Integer -> IO Result
onlinePush proc number = onlineFun proc ( CmdPush number )

onlinePop :: Process -> Integer -> IO Result
onlinePop proc number = onlineFun proc ( CmdPop number )

onlineAssert :: Process -> Expr -> IO Result
onlineAssert proc expression = onlineFun proc ( CmdAssert expression)

onlineCheckSat :: Process  -> IO Result
onlineCheckSat proc = onlineFun proc CmdCheckSat

onlineGetAssertions :: Process -> IO Result
onlineGetAssertions proc = onlineFun proc  CmdGetAssertions

onlineGetValue :: Process -> [Expr] -> IO Result
onlineGetValue proc exprs = onlineFun proc ( CmdGetValue exprs)

onlineGetProof :: Process -> IO Result
onlineGetProof proc = onlineFun proc  CmdGetProof

onlineGetUnsatCore :: Process -> IO Result
onlineGetUnsatCore proc = onlineFun proc CmdGetUnsatCore

onlineGetInfo :: Process -> InfoFlag -> IO Result
onlineGetInfo proc info = onlineFun proc ( CmdGetInfo info )

onlineGetOption :: Process -> Name -> IO Result
onlineGetOption proc name = onlineFun proc ( CmdGetOption name )

onlineExit :: Process -> IO Result
onlineExit proc = onlineFun proc CmdExit
