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

onlineGenResponse :: Process -> Command -> IO GenResult
onlineGenResponse proc cmd  = liftA genResponse (onlineFun proc cmd)

onlineCheckSatResponse :: Process -> Command -> IO SatResult
onlineCheckSatResponse proc cmd = liftA checkSatResponse (onlineFun proc cmd)

onlineGetValueResponse :: Process -> Command -> IO GValResult
onlineGetValueResponse proc cmd = liftA getValueResponse (onlineFun proc cmd)


--SMT Commands.

onlineSetLogic :: Process -> Name -> IO GenResult
onlineSetLogic proc name = onlineGenResponse proc (CmdSetLogic name)

onlineSetOption :: Process -> Option -> IO GenResult
onlineSetOption proc option = onlineGenResponse proc (CmdSetOption option)

onlineSetInfo :: Process ->  Attr -> IO GenResult
onlineSetInfo proc attr  = onlineGenResponse proc  (CmdSetInfo attr)

onlineDeclareType :: Process -> Name -> Integer -> IO GenResult
onlineDeclareType proc name number =
    onlineGenResponse proc (CmdDeclareType name number)

onlineDefineType :: Process -> Name -> [Name] -> Type -> IO GenResult
onlineDefineType proc name names t =
    onlineGenResponse proc (CmdDefineType name names t)

onlineDeclareFun :: Process -> Name -> [Type] -> Type -> IO GenResult
onlineDeclareFun proc name lt t =
    onlineGenResponse proc (CmdDeclareFun name lt t)

onlineDefineFun :: Process -> Name -> [Binder] -> Type -> Expr -> IO GenResult
onlineDefineFun proc name binders t expression =
    onlineGenResponse proc (CmdDefineFun name binders t expression)

onlinePush :: Process -> Integer -> IO GenResult
onlinePush proc number = onlineGenResponse proc (CmdPush number)

onlinePop :: Process -> Integer -> IO GenResult
onlinePop proc number = onlineGenResponse proc (CmdPop number)

onlineAssert :: Process -> Expr -> IO GenResult
onlineAssert proc expression = onlineGenResponse proc (CmdAssert expression)

onlineCheckSat :: Process  -> IO SatResult
onlineCheckSat proc = onlineCheckSatResponse proc CmdCheckSat

onlineGetAssertions :: Process -> IO String
onlineGetAssertions proc = onlineFun proc  CmdGetAssertions

onlineGetValue :: Process -> [Expr] -> IO GValResult
onlineGetValue proc exprs = onlineGetValueResponse proc (CmdGetValue exprs)

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
