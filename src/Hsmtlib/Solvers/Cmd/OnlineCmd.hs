{- |
Module      : OnlineCmd

Module with the functions used in online Mode.
-}

module Hsmtlib.Solvers.Cmd.OnlineCmd where
import           Control.Applicative                  (liftA)
import           Hsmtlib.Solver
import           Hsmtlib.Solvers.Cmd.CmdResult
import           Hsmtlib.Solvers.Cmd.ProcCom.Process
import           Smtlib.Syntax.Syntax                  
import           Smtlib.Syntax.ShowSL

--Uses the function  send from Cmd.Solver to send the command.
onlineFun ::  Process  -> Command -> Solvers ->  IO String
onlineFun proc cmd Cvc4 = sendCvc4 proc ((showSL cmd) ++ "\n") 
onlineFun proc cmd _ = send proc ((showSL cmd) ++ "\n") 


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

onlineSetLogic ::Solvers -> Process -> String -> IO Result
onlineSetLogic solver proc name = 
    onlineGenResponse proc (SetLogic name) solver

onlineSetOption ::Solvers -> Process -> Option -> IO Result
onlineSetOption solver proc option = 
    onlineGenResponse proc (SetOption option) solver

onlineSetInfo ::Solvers -> Process -> Attribute -> IO Result
onlineSetInfo solver proc attr  = 
    onlineGenResponse proc (SetInfo attr) solver

onlineDeclareSort ::Solvers -> Process -> String -> Int -> IO Result
onlineDeclareSort solver proc name number =
    onlineGenResponse proc (DeclareSort name number) solver

onlineDefineSort ::Solvers -> Process -> String -> [String] -> Sort -> IO Result
onlineDefineSort solver proc name names t =
    onlineGenResponse proc (DefineSort name names t) solver

onlineDeclareFun ::Solvers -> Process -> String -> [Sort] -> Sort -> IO Result
onlineDeclareFun solver proc name lt t =
    onlineGenResponse proc (DeclareFun name lt t) solver

onlineDefineFun :: Solvers 
                -> Process 
                -> String 
                -> [SortedVar] 
                -> Sort 
                -> Term -> IO Result
onlineDefineFun solver proc name binders t expression =
    onlineGenResponse proc (DefineFun name binders t expression) solver

onlinePush ::Solvers -> Process -> Int -> IO Result
onlinePush solver proc number = 
    onlineGenResponse proc (Push number) solver

onlinePop ::Solvers -> Process -> Int -> IO Result
onlinePop solver proc number = 
    onlineGenResponse proc (Pop number) solver

onlineAssert ::Solvers -> Process -> Term -> IO Result
onlineAssert solver proc expression = 
    onlineGenResponse proc (Assert expression) solver

onlineCheckSat ::Solvers -> Process  -> IO Result
onlineCheckSat solver proc = 
    onlineCheckSatResponse proc CheckSat solver

onlineGetAssertions ::Solvers -> Process -> IO Result
onlineGetAssertions solver proc = 
    onlineGetAssertionResponse proc GetAssertions solver

onlineGetValue ::Solvers -> Process -> [Term] -> IO Result
onlineGetValue solver proc exprs = 
    onlineGetValueResponse proc (GetValue exprs) solver

onlineGetProof ::Solvers -> Process -> IO Result
onlineGetProof solver proc = 
    onlineGetProofResponse proc GetProof solver

onlineGetUnsatCore ::Solvers -> Process -> IO Result
onlineGetUnsatCore solver proc = 
    onlineGetUnsatCoreResponse proc GetUnsatCore solver

onlineGetInfo ::Solvers -> Process -> InfoFlags -> IO Result
onlineGetInfo solver proc info = 
    onlineGetInfoResponse proc (GetInfo info) solver

onlineGetOption ::Solvers -> Process -> String -> IO Result
onlineGetOption solver proc name = 
    onlineGetOptionResponse proc (GetOption name) solver

onlineExit :: Process -> IO Result
onlineExit proc =  onlineExitResponse proc
