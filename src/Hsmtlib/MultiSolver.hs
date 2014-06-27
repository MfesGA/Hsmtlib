{- |
  Module: Hsmtlib.MultiSolver
  This Module provide functions to interact with many solvers at the same time.
-}

module Hsmtlib.MultiSolver where

import           Hsmtlib.Solver            as Slv
import           SMTLib2
import           Hsmtlib(startSolver)




{- |  functions that trades the argument order for easy implementation of the functions on this module -}
flipper5 f b c d e a  = f a b c d e 

flipper5' f a b c d e = f b c d e a

flipper3 f b c a = f a b c 

flipper3' f a b c = f c b a 

flipper4  f  b c d a=  f a b c d

flipper4' f a b c d = f b c d a    

{- | function that allow to start many solvers at once -}
mStartSolver' :: Mode-> Logic-> Maybe SolverConfig -> Maybe String -> [Solvers]-> [IO Solver]
mStartSolver' mode logic conf file = map $ (flipper5 startSolver mode logic conf file)  

{- | function that allow to start many solvers at once with the same argument order thats used in the rest of the libary-}
mStartSolver = flipper5' $ mStartSolver'


mSetLogic' name  = map $ (flip setLogic name) 
mSetLogic  = flip $ mSetLogic' 

mSetOption' op = map $ (flip setOption op) 
mSetOption = flip $ mSetOption'

mSetInfo' inf= map $ (flip setInfo inf)
mSetInfo = flip $ mSetInfo'

mDeclareType' name inte = map $ (flipper3 declareType name inte) 
mDeclareType = flipper3' $ mDeclareType' 

mDefineType' name namel tipe= map $ (flipper4 defineType name namel tipe)
mDefineType = flipper4' $ mDefineType'

mDeclareFun' name namel tipe= map $ (flipper4 declareFun name namel tipe)
mDeclareFun = flipper4' $ mDeclareFun'

mDefineFun' name bind tipe expr= map $ (flipper5 defineFun name bind tipe expr)
mDefineFun = flipper5' $ mDefineFun'

mPush' int= map $ (flip push int)
mPush = flip $ mPush'

mPop' int= map $ (flip pop int)
mPop = flip $ mPop'

mAssert' expr= map $ (flip assert expr)
mAssert = flip $ mAssert'

mCheckSat = map $ checkSat 

mGetAssertions = map $ getAssertions

mGetValue' expr= map $ (flip getValue expr)
mGetValue = flip $ mGetValue'

mGetProof = map $ getProof

mGetUnsatCore = map $ getUnsatCore

mGetInfo' expr= map $ (flip getInfo expr)
mGetInfo = flip $ mGetInfo'

mGetOption' expr= map $ (flip getOption expr)
mGetOption = flip $ mGetOption'

mExit =  map $ exit


