Hsmtl provides functions to interact with several smt solvers using SMT-LIB 2.

Current supported solvers and avaliable interaction modes in each other.


| Solver | Online | Script |
|--------|--------|--------|
| Cvc4   | X      | X      |
| Z3     | X      | X      |
| Alt-Ergo|       | X      |
| Yices* | X      | X      |
| MathSat| X      |        |


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
```
