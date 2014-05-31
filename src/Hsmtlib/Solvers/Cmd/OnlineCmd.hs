{- |
Module      : OnlineCmd

Module with the functions used in online Mode.
-}

module Hsmtlib.Solvers.Cmd.OnlineCmd where
import           Control.Applicative                  (liftA)
import           Hsmtlib.Solver
import           Hsmtlib.Solvers.Cmd.CmdResult
import           Hsmtlib.Solvers.Cmd.ProcCom.Process
import           SMTLib2
import           Text.PrettyPrint



--Uses the function  send from Cmd.Solver to send the command.
onlineFun ::  Process  -> Command -> Solvers ->  IO String
onlineFun proc cmd Cvc4 = sendCvc4 proc (render (pp  cmd) ++ "\n") 
onlineFun proc cmd _ = send proc (render (pp  cmd) ++ "\n") 



onExit :: Process -> IO String
onExit proc = endProcess proc >> return "success"

onlineGenResponse :: Process -> Command -> Solvers -> IO Result
onlineGenResponse proc cmd solver  = 
    liftA genResponse (onlineFun proc cmd solver)

onlineCheckSatResponse :: Process -> Command -> Solvers -> IO Result
onlineCheckSatResponse proc cmd solver = 
    liftA checkSatResponse (onlineFun proc cmd solver) 

onlineGetValueResponse :: Process -> Command -> Solvers -> IO Result
onlineGetValueResponse proc cmd solver = 
    liftA getValueResponse (onlineFun proc cmd solver)


onlineGetInfoResponse :: Process -> Command -> Solvers -> IO Result
onlineGetInfoResponse proc cmd solver = 
    liftA getInfoResponse (onlineFun proc cmd solver)


onlineGetAssertionResponse :: Process -> Command -> Solvers -> IO Result
onlineGetAssertionResponse proc cmd solver =
    liftA getAssertionResponse (onlineFun proc cmd solver)

onlineGetProofResponse :: Process -> Command ->  Solvers -> IO Result
onlineGetProofResponse proc cmd solver = 
    liftA getProofResponse (onlineFun proc cmd solver)


onlineGetUnsatCoreResponse :: Process -> Command -> Solvers -> IO Result
onlineGetUnsatCoreResponse proc cmd solver = 
    liftA getUnsatCoreResponse (onlineFun proc cmd solver)


onlineGetAssignmentResponse :: Process -> Command -> Solvers -> IO Result
onlineGetAssignmentResponse proc cmd solver = 
    liftA getAssignmentResponse (onlineFun proc cmd solver)


onlineGetOptionResponse :: Process -> Command -> Solvers -> IO Result
onlineGetOptionResponse proc cmd solver = 
    liftA getOptionResponse (onlineFun proc cmd solver)

onlineExitResponse :: Process -> IO Result
onlineExitResponse proc = liftA genResponse (onExit proc)

--SMT Commands.

onlineSetLogic ::Solvers -> Process -> Name -> IO Result
onlineSetLogic solver proc name = 
    onlineGenResponse proc (CmdSetLogic name) solver

onlineSetOption ::Solvers -> Process -> Option -> IO Result
onlineSetOption solver proc option = 
    onlineGenResponse proc (CmdSetOption option) solver

onlineSetInfo ::Solvers -> Process ->  Attr -> IO Result
onlineSetInfo solver proc attr  = 
    onlineGenResponse proc (CmdSetInfo attr) solver

onlineDeclareType ::Solvers -> Process -> Name -> Integer -> IO Result
onlineDeclareType solver proc name number =
    onlineGenResponse proc (CmdDeclareType name number) solver

onlineDefineType ::Solvers -> Process -> Name -> [Name] -> Type -> IO Result
onlineDefineType solver proc name names t =
    onlineGenResponse proc (CmdDefineType name names t) solver

onlineDeclareFun ::Solvers -> Process -> Name -> [Type] -> Type -> IO Result
onlineDeclareFun solver proc name lt t =
    onlineGenResponse proc (CmdDeclareFun name lt t) solver

onlineDefineFun ::Solvers -> Process -> Name -> [Binder] -> Type -> Expr -> IO Result
onlineDefineFun solver proc name binders t expression =
    onlineGenResponse proc (CmdDefineFun name binders t expression) solver

onlinePush ::Solvers -> Process -> Integer -> IO Result
onlinePush solver proc number = 
    onlineGenResponse proc (CmdPush number) solver

onlinePop ::Solvers -> Process -> Integer -> IO Result
onlinePop solver proc number = 
    onlineGenResponse proc (CmdPop number) solver

onlineAssert ::Solvers -> Process -> Expr -> IO Result
onlineAssert solver proc expression = 
    onlineGenResponse proc (CmdAssert expression) solver

onlineCheckSat ::Solvers -> Process  -> IO Result
onlineCheckSat solver proc = 
    onlineCheckSatResponse proc CmdCheckSat solver

onlineGetAssertions ::Solvers -> Process -> IO Result
onlineGetAssertions solver proc = 
    onlineGetAssertionResponse proc  CmdGetAssertions solver

onlineGetValue ::Solvers -> Process -> [Expr] -> IO Result
onlineGetValue solver proc exprs = 
    onlineGetValueResponse proc (CmdGetValue exprs) solver

onlineGetProof ::Solvers -> Process -> IO Result
onlineGetProof solver proc = 
    onlineGetProofResponse proc  CmdGetProof solver

onlineGetUnsatCore ::Solvers -> Process -> IO Result
onlineGetUnsatCore solver proc = 
    onlineGetUnsatCoreResponse proc CmdGetUnsatCore solver

onlineGetInfo ::Solvers -> Process -> InfoFlag -> IO Result
onlineGetInfo solver proc info = 
    onlineGetInfoResponse proc (CmdGetInfo info) solver

onlineGetOption ::Solvers -> Process -> Name -> IO Result
onlineGetOption solver proc name = 
    onlineGetOptionResponse proc (CmdGetOption name) solver

onlineExit :: Process -> IO Result
onlineExit proc =  onlineExitResponse proc
