module ContextCmd where


import           SMTLib2
import           System.IO        (Handle, hClose, hFlush, hPutStr)
import           Text.PrettyPrint
import           Process





-- | Context is just a string wich will be sent to std_in
type Context = String
type CtResult=(IO Result,ContextArgs)
type Result = String

data ContextArgs = ContextArgs
    { handle   :: Handle
    , cmdPath  :: CmdPath
    , args     :: Args
    , context  :: Context
    }

tocontext:: ContextArgs -> Command -> ContextArgs
tocontext ct cmd = ContextArgs{
			handle = handle ct,
			cmdPath = cmdPath ct,
			args= args ct,
			context= (context ct) ++(render ( pp  cmd ) ++ "\n")
			 }


contextnexecFun ::  ContextArgs  -> Command ->  CtResult
contextnexecFun ct cmd =  (return "", (tocontext ct cmd))

contextexecFun :: ContextArgs -> Command -> CtResult 
contextexecFun ct cmd = ( (sendContext (cmdPath ct) (args ct) (context (tocontext ct cmd))),ct)     

contextSetLogic ::  Name -> ContextArgs -> CtResult
contextSetLogic  name ct = contextnexecFun ct ( CmdSetLogic name )


contextSetOption ::  Option-> ContextArgs  -> CtResult
contextSetOption  option proc = contextnexecFun proc ( CmdSetOption option)


contextSetInfo :: Attr -> ContextArgs -> CtResult
contextSetInfo  attr proc = contextnexecFun proc  (CmdSetInfo attr)


contextDeclareType ::  Name -> Integer -> ContextArgs -> CtResult
contextDeclareType  name number proc= contextnexecFun proc ( CmdDeclareType name number)

contextDefineType ::  Name -> [Name] -> Type ->ContextArgs ->  CtResult
contextDefineType  name names t proc= contextnexecFun proc ( CmdDefineType name names t )

contextDeclareFun ::  Name -> [Type] -> Type ->ContextArgs -> CtResult
contextDeclareFun  name lt t proc= contextnexecFun  proc ( CmdDeclareFun name lt t )

contextDefineFun ::  Name -> [Binder] -> Type -> Expr -> ContextArgs -> CtResult
contextDefineFun  name binders t expression proc= contextnexecFun proc ( CmdDefineFun name binders t expression)

contextPush ::  Integer ->ContextArgs  -> CtResult
contextPush  number proc= contextnexecFun proc ( CmdPush number )

contextPop :: Integer ->ContextArgs ->  CtResult
contextPop  number proc= contextnexecFun proc ( CmdPop number )

contextAssert ::  Expr -> ContextArgs -> CtResult
contextAssert  expression proc= contextnexecFun proc ( CmdAssert expression)


contextCheckSat :: ContextArgs  -> CtResult
contextCheckSat proc = contextexecFun proc CmdCheckSat

contextGetAssertions :: ContextArgs -> CtResult
contextGetAssertions proc = contextexecFun proc  CmdGetAssertions

contextGetValue ::  [Expr] ->ContextArgs -> CtResult
contextGetValue exprs proc = contextexecFun proc ( CmdGetValue exprs)

contextGetProof :: ContextArgs -> CtResult
contextGetProof proc = contextexecFun proc  CmdGetProof

contextGetUnsatCore :: ContextArgs -> CtResult
contextGetUnsatCore proc = contextexecFun proc CmdGetUnsatCore

contextGetInfo ::  InfoFlag ->ContextArgs -> CtResult
contextGetInfo  info proc= contextexecFun proc ( CmdGetInfo info )

contextGetOption ::  Name -> ContextArgs ->CtResult
contextGetOption  name proc = contextexecFun proc ( CmdGetOption name )

contextExit :: ContextArgs -> CtResult
contextExit proc = contextexecFun proc CmdExit


(#) ::  CtResult -> (ContextArgs -> CtResult) -> CtResult
(#)  ct f = do
	print <- fst ct
	   
	f $ snd(ct)





