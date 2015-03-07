module Hsmtlib.Solvers.Cmd.CmdResult where


import           Prelude                            as P
import           Data.Map                           as M
import           Data.Maybe                         (isJust)
import           Control.Monad
import           Control.Applicative                ((<|>))
import           Text.ParserCombinators.Parsec.Prim (parse)



import           Smtlib.Parsers.ResponseParsers
import           Smtlib.Syntax.Syntax             as S
import           Hsmtlib.Solver                     as Solv





{-
   #########################################################################
   #                                                                       #
   #                             Gen Response                              #
   #                                                                       #
   #########################################################################
-}



genResponse :: String -> Result
genResponse stg =
    case result of
        Left err -> ComError $ stg ++ " | " ++ show err
        Right cmdRep -> CGR cmdRep
    where result = parse parseGenResponse "" stg




{-
   #########################################################################
   #                                                                       #
   #                      Check Sat Response                               #
   #                                                                       #
   #########################################################################
-}

checkSatResponse :: String -> Result
checkSatResponse stg =
    case result of
        Left err ->  ComError $ stg ++  " | " ++ show  err
        Right cmdRep ->  CCS cmdRep
    where result = parse parseCheckSatResponse "" stg




{-
   #########################################################################
   #                                                                       #
   #                          get Value Respose                            #
   #                                                                       #
   #########################################################################
-}

getValueResponse :: String -> Result
getValueResponse stg = case result of
                        Left err ->  ComError $ stg ++  " | " ++  show err
                        Right vals -> CGV vals
    where result = parse parseGetValueResponse "" stg




{- 
   #########################################################################
   #                                                                       #
   #                          get Info Response                            #
   #                                                                       #
   #########################################################################
-}

getInfoResponse :: String  -> Result
getInfoResponse stg = case result of
                        Left err ->  ComError $ stg ++  " | " ++  show err
                        Right vals -> CGI vals
          where result = parse parseGetInfoResponse "" stg


{- 
   #########################################################################
   #                                                                       #
   #                          get Assertion response                       #
   #                                                                       #
   #########################################################################
-}


getAssertionResponse :: String  -> Result
getAssertionResponse stg = case result of
                        Left err ->  ComError $ stg ++  " | " ++  show err
                        Right vals -> CGAssert vals
          where result = parse parseGetAssertionsResponse "" stg



{- 
   #########################################################################
   #                                                                       #
   #                          get Proof response                           #
   #                                                                       #
   #########################################################################
-}


getProofResponse :: String  -> Result
getProofResponse stg = case result of
                        Left err ->  ComError $ stg ++  " | " ++  show err
                        Right vals -> CGP  vals
          where result = parse parseGetProofResponse "" stg



{- 
   #########################################################################
   #                                                                       #
   #                          get Unsat Core Response                      #
   #                                                                       #
   #########################################################################
-}


getUnsatCoreResponse :: String  -> Result
getUnsatCoreResponse stg = case result of
                        Left err ->  ComError $ stg ++  " | " ++  show err
                        Right vals -> CGUC vals
          where result = parse parseGetUnsatCoreResp "" stg



{- 
   #########################################################################
   #                                                                       #
   #                          get  Assignment                              #
   #                                                                       #
   #########################################################################
-}


getAssignmentResponse :: String  -> Result
getAssignmentResponse stg = case result of
                        Left err ->  ComError $ stg ++  " | " ++  show err
                        Right vals -> CGAssig vals
          where result = parse parseGetAssignmentResp "" stg


{- 
   #########################################################################
   #                                                                       #
   #                          get  Option                                  #
   #                                                                       #
   #########################################################################
-}


getOptionResponse :: String  -> Result
getOptionResponse stg = case result of
                        Left err ->  ComError $ stg ++  " | " ++  show err
                        Right vals -> CGO vals
          where result = parse parseGetOptionResponse "" stg