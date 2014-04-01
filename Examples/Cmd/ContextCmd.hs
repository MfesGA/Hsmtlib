{- |
Module      : ContextCmd

Module with the functions used in context Mode.
-}
module Cmd.ContextCmd where

import           Cmd.ProcCom.Process
import           SMTLib2
import           Cmd.Solver
import           Text.PrettyPrint

--Creates a string with the command.
renderCtx :: Command -> [String]
renderCtx cmd = [render (pp  cmd) ++ "\n"]

{-
  Since the logic is set when a solver is created and we dont know which
  command will be used first we passed the logic to some functions.
  This way the commands that execute and need the logic set before
  veryfi if the logic is already in context if not, they put it in the
  head of the list.
-}

{-|
Receives  a command, might receive a logic, a preious context
('Cmd.Solver.CtResult') and returns a new one.


Creates the new comand and returns a new context with the previous one and
the new command.
-}
ctxNExecFun :: Command -> Maybe String ->  IO CtResult -> IO CtResult
ctxNExecFun  cmd (Just logic) prevCt = do
  rec <- prevCt
  let cLogic = renderCtx (CmdSetLogic (N logic)) -- crete the logic command
  -- new Context, previous context + new command
  let nContext = context rec ++ renderCtx cmd
  -- if logic isn't set than put in the head of the list. Return the new
  -- Context.
  if head cLogic `elem`  nContext then newCtxRes nContext  [] else
    newCtxRes (cLogic ++ nContext) ""

ctxNExecFun cmd Nothing prevCt = do
    rec <- prevCt
    newCtxRes (context rec ++ renderCtx cmd) ""


--Creates a new context.
newCtxRes :: [String ] -> String -> IO CtResult
newCtxRes nContext nResult =
  return CtxRes{ context = nContext
               , result = nResult
               }

ctxExecFun :: CmdPath -> Args -> Command -> IO CtResult -> IO CtResult
ctxExecFun cmdPath fargs command rec  = do
    prevCt <- rec
    -- new Context, previous context + new command
    let ctx_send = context prevCt ++ renderCtx command
    -- Create solver and execute with the context. Wait for anwser.
    res <- sendContext cmdPath fargs  (concat ctx_send)
    --return new context with answer.
    return  CtxRes { context = ctx_send, result =  last $ lines res}


--SMT Commands.

-- ctxCheckSat, ctxGetAssertins, ctxGetValue, ctxGetValue,
-- ctxGetProof, ctxGetUnsatCore, ctxGetInfo, ctxGetOption,
-- ctxExit.

--All above functions use ctxExcFun the rest use ctxNExcFun.

ctxSetLogic :: Name -> IO CtResult -> IO CtResult
ctxSetLogic name = ctxNExecFun (CmdSetLogic name) Nothing


ctxSetOption :: Option -> IO CtResult -> IO CtResult
ctxSetOption  option = ctxNExecFun (CmdSetOption option) Nothing


ctxSetInfo :: Attr -> IO CtResult -> IO CtResult
ctxSetInfo  attr = ctxNExecFun (CmdSetInfo attr) Nothing

ctxDeclareType :: String -> Name -> Integer -> IO CtResult -> IO CtResult
ctxDeclareType logic name number =
    ctxNExecFun (CmdDeclareType name number) (Just logic)

ctxDefineType :: String -> Name -> [Name] -> Type -> IO CtResult -> IO CtResult
ctxDefineType logic  name names t =
    ctxNExecFun (CmdDefineType name names t) (Just logic)

ctxDeclareFun :: String -> Name -> [Type] -> Type -> IO CtResult -> IO CtResult
ctxDeclareFun logic name lt t =
    ctxNExecFun (CmdDeclareFun name lt t) (Just logic)

ctxDefineFun :: String ->Name -> [Binder] -> Type -> Expr -> IO CtResult -> IO CtResult
ctxDefineFun logic name binders t expression =
    ctxNExecFun (CmdDefineFun name binders t expression) (Just logic)

ctxPush :: Integer -> IO CtResult  -> IO CtResult
ctxPush number = ctxNExecFun (CmdPush number) Nothing

ctxPop :: Integer -> IO CtResult -> IO CtResult
ctxPop number = ctxNExecFun (CmdPop number) Nothing

ctxAssert :: Expr -> IO CtResult -> IO CtResult
ctxAssert expression = ctxNExecFun (CmdAssert expression) Nothing

ctxCheckSat :: CmdPath -> Args -> IO CtResult -> IO CtResult
ctxCheckSat cmdPath fargs = ctxExecFun  cmdPath fargs CmdCheckSat

ctxGetAssertions :: CmdPath -> Args -> IO CtResult -> IO CtResult
ctxGetAssertions cmdPath fargs =
    ctxExecFun cmdPath fargs CmdGetAssertions

ctxGetValue :: CmdPath -> Args -> [Expr] -> IO CtResult -> IO CtResult
ctxGetValue cmdPath fargs exprs =
    ctxExecFun cmdPath fargs (CmdGetValue exprs)

ctxGetProof :: CmdPath -> Args -> IO CtResult -> IO CtResult
ctxGetProof cmdPath fargs =
    ctxExecFun cmdPath fargs CmdGetProof

ctxGetUnsatCore :: CmdPath -> Args -> IO CtResult -> IO CtResult
ctxGetUnsatCore cmdPath fargs =
    ctxExecFun cmdPath fargs CmdGetUnsatCore

ctxGetInfo :: CmdPath -> Args -> InfoFlag -> IO CtResult -> IO CtResult
ctxGetInfo cmdPath fargs info =
    ctxExecFun cmdPath fargs (CmdGetInfo info)

ctxGetOption :: CmdPath -> Args -> Name -> IO CtResult -> IO CtResult
ctxGetOption cmdPath fargs name =
    ctxExecFun cmdPath fargs (CmdGetOption name)

ctxExit :: CmdPath -> Args -> IO CtResult -> IO CtResult
ctxExit cmdPath fargs =
    ctxExecFun cmdPath fargs CmdExit


