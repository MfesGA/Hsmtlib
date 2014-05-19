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

There exists two modes to interact with a solver:

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

* Batch executiton allows the solver to read an entire context and execute it  



A few tutorials on how to use this library can be found in the [wiki] (https://github.com/MfesGA/Hsmtlib/wiki).
