module Hsmtlib.Solvers.Cmd.CmdResult where

import           Control.Applicative                ((<|>))
import           Control.Monad
--import           Data.List                              (intercalate)
import           Data.Map                           as M
import           Data.Maybe                         (isJust)

import           Smtlib.Parsers.ResponseParsers
import           Smtlib.Syntax.Syntax             as S

import           Hsmtlib.Solver                     as Solv
import           Hsmtlib.Solvers.Cmd.ResultHelpers

import           Prelude                            as P
import           Text.ParserCombinators.Parsec.Prim (parse)



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



{-
getValueResponse :: String -> GValResult
getValueResponse stg = GVUError  $ stg ++ "\n" ++ tree
    where result = parse parseGetValueResponse "" stg
          tree =  getVR' result


getVR' :: Show a => Either a [ValuationPair] -> String
getVR' result =
  case result of
    Left err -> show err
    Right vals -> intercalate "\n" (fmap (showValuationPair 0) vals)

-}

getValueResponse :: String -> Result
getValueResponse stg = case result of
                        Left err ->  ComError $ stg ++  " | " ++  show err
                        Right vals -> getValResponse vals
    where result = parse parseGetValueResponse "" stg



getValResponse::[ValuationPair] -> Result
getValResponse vp = CGV $  arrays ++ [errors]
                      -- gets the results in a Maybe GValResult.
                where res = getValResponses vp
                      --Produes UErrors from the results that gave Nothing.
                      errors = Synt $ valErrors' vp res
                      cRes = P.filter isJust res -- Removes the Nothings.
                      -- Joins all arrays in one and
                      --removes the Just from all results.
                      arrays = joinArrays cRes



valErrors' :: [ValuationPair] -> [Maybe GValResult] -> [ValuationPair]
valErrors' [] [] = []
valErrors' [] (_:_) = []
valErrors' (_:_) [] = []
valErrors' (x:xs) (Nothing:gs) =
  x : valErrors' xs gs
valErrors' (_:xs) (_:gs) = valErrors' xs  gs



joinArrays :: [Maybe GValResult] -> [GValResult]
joinArrays = joinArrays' $ VArrays empty


joinArrays' :: GValResult -> [Maybe GValResult] -> [GValResult]
-- if there was no array in the result then don't put an empty one.
joinArrays' (VArrays n) [] | M.null n = []
                           | otherwise = [VArrays n]
joinArrays' res (x:xs) = case nVal of
                          (VArrays _) -> joinArrays' nVal xs
                          _ -> nVal : joinArrays' res xs
                         where nVal = checkGVal res x
joinArrays' _ _ = []

checkGVal :: GValResult ->  Maybe GValResult -> GValResult
checkGVal (VArrays oarr) (Just(VArrays arr)) =
  VArrays $ unionWith union oarr arr
checkGVal _ (Just x) = x
checkGVal _ _ = VArrays empty


getValResponses :: [ValuationPair] -> [Maybe GValResult]
getValResponses = fmap getGValResult

getGValResult :: ValuationPair -> Maybe GValResult
getGValResult vp = getVar vp
               <|> getArray vp
               <|> getFun vp
               <|> getBitVec vp


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
          where result = parse parseGetAssertionResponse "" stg



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