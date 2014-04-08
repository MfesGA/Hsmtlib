Hsmtl provides functions to interact with several smt solvers using SMT-LIB 2.

Current supported solvers and avaliable interaction modes in each other.


| Solver | Online | Script | Context |
|--------|--------|--------|---------|
| Cvc4   | X      | X      | X       |
| Z3     | X      | X      | X       |
| Altergo|        | X      | X       |
| Yices* | X      | X      | X       |
| MathSat|        |        |         |

 (* using yices-smt2 available in: http://yices.csl.sri.com/download-yices2.shtml)
 
There exists three modes to interact with a solver:

* Online:
 
  > In online Mode a solver is creted and kept running.
  Commands are sent via pipe one by one and every time one 
   is sent it also reads the answer of the solver.

* Script:

    >In script Mode a file is created in a desired file path, 
    if Nothing is passed then its created in the current directory 
    with the name temp.smt2. If a file already exists then it's overwriten.
    
    >The functions in this mode behave in the following manner: 
    If it's a funcion where something is declared, for example declareFun or 
    assert then it's only writen to the file. In functions where some feedback 
    is expected such as checkSat, this are writen to the file, a solver is 
    created and the file is given to solver, and it waits for the result. 
    The result is the result of the last function.

* Context:

    >In context Mode two lists are kept, a list with all the commands given and
    a list that has the results of commands that demand some output 
    from the solver.

    >The functions in this mode behave has in script Mode being the diference 
    that instead of a file the commands are kept in the list.
    
    >In order to use the context mode it's nedded some special operators that
    aren't nedded in other modes.
    

Example of Online Mode:

```haskell
main :: IO ()
main = do
  solver <- startSolver Z3 Online "QF_LIA"  Nothing Nothing
  declareFun solver (N "a") [] tInt >>= print
  declareFun solver (N "x") [] tInt >>= print
  declareFun solver (N "y") [] tInt >>= print
  declareFun solver (N "f") [] tInt >>= print
  checkSat solver >>= print
  exit solver >>= print
```

Example of Context Mode:

```haskell
main :: IO ()
main = do
  solver <- startSolver Cvc4 Slv.Context "QF_LIA"  Nothing Nothing
  declareFunCt solver (N "a") [] tInt |*|
  declareFunCt solver (N "x") [] tInt |#|
  declareFunCt solver (N "y") [] tInt |#|
  checkSatCt solver |#|
  exitCt solver >>= print
```

Important functions to use the library:


```haskell
startSolver :: Solvers -- Avaliable Solvers.
            -> Mode -- Avaliable Modes, Online, Script, Context.
            -> String -- The desired SMT Logic.
            -> Maybe SolverConfig -- A customized Configuration for the Solver.
            -> Maybe FilePath -- A possible alternate path to save the Script.
            -> IO Solver

{-
 Solver data type that has all the functions.
 There exists two Constructors Solver and CtSolver, Solver is used for Online
 and Script mode. CtSolver is only used in ContextMode.

 The types of both are similar the only diferenc is that in CtSolver we need
 too keep the list of all commands used, so each commands needs to receive
 the list of the last command.

 To facilitate this there existis some combinators further bellow,
-}
data Solver = Solver
    { setLogic      :: Name -> IO Result
    , setOption     :: Option -> IO Result
    , setInfo       :: Attr -> IO Result
    , declareType   :: Name -> Integer -> IO Result
    , defineType    :: Name -> [Name] -> Type -> IO Result
    , declareFun    :: Name -> [Type] -> Type -> IO Result
    , defineFun     :: Name -> [Binder] -> Type -> Expr -> IO Result
    , push          :: Integer -> IO Result
    , pop           :: Integer -> IO Result
    , assert        :: Expr -> IO Result
    , checkSat      :: IO Result
    , getAssertions :: IO Result
    , getValue      :: [Expr]-> IO Result
    , getProof      :: IO Result
    , getUnsatCore  :: IO Result
    , getInfo       :: InfoFlag -> IO Result
    , getOption     :: Name -> IO Result
    , exit          :: IO Result
    }
    | CtSolver
    { setLogicCt      :: Name -> IO CtResult -> IO CtResult
    , setOptionCt     :: Option -> IO CtResult -> IO CtResult
    , setInfoCt       :: Attr -> IO CtResult -> IO CtResult
    , declareTypeCt   :: Name -> Integer -> IO CtResult -> IO CtResult
    , defineTypeCt    :: Name -> [Name] -> Type -> IO CtResult -> IO CtResult
    , declareFunCt    :: Name -> [Type] -> Type -> IO CtResult -> IO CtResult
    , defineFunCt     :: Name -> [Binder] -> Type -> Expr -> IO CtResult -> IO CtResult
    , pushCt          :: Integer -> IO CtResult -> IO CtResult
    , popCt           :: Integer -> IO CtResult -> IO CtResult
    , assertCt        :: Expr -> IO CtResult -> IO CtResult
    , checkSatCt      :: IO CtResult -> IO CtResult
    , getAssertionsCt :: IO CtResult -> IO CtResult
    , getValueCt      :: [Expr] -> IO CtResult -> IO CtResult
    , getProofCt      :: IO CtResult -> IO CtResult
    , getUnsatCoreCt  :: IO CtResult -> IO CtResult
    , getInfoCt       :: InfoFlag -> IO CtResult -> IO CtResult
    , getOptionCt     :: Name -> IO CtResult -> IO CtResult
    , exitCt          :: IO CtResult -> IO CtResult
    }

{-
  This function should be used after the first function of context mode.
  It passes an empty context to the first function and passes the result
  of the first function to the second.
-}
(|*|) :: (IO CtResult -> IO CtResult) -> (IO CtResult -> IO CtResult) -> IO CtResult

-- This function is used between functions.
(|#|) :: IO CtResult -> (IO CtResult -> IO CtResult) -> IO CtResult


```
