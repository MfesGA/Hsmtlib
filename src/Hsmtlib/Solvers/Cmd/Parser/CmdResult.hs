module  Hsmtlib.Solvers.Cmd.Parser.CmdResult where

--import           Control.Applicative                    hiding (empty)
import           Control.Monad
import           Data.List                              (intercalate)
import           Data.Map
import           Hsmtlib.Solver                         as Solv
import           Hsmtlib.Solvers.Cmd.Parser.Parsers     hiding (numeral, symbol)
import           Hsmtlib.Solvers.Cmd.Parser.Syntax      as S
import           Hsmtlib.Solvers.Cmd.Parser.Visualizers
import           Text.ParserCombinators.Parsec.Prim     (parse)

genResponse :: String -> GenResult
genResponse stg =
    case result of
        Left err -> Solv.GUError $ show err
        Right cmdRep -> case cmdRep of
                            CmdGenResponse S.Success -> Solv.Success
                            CmdGenResponse S.Unsupported -> Solv.Unsupported
                            CmdGenResponse (S.Error err) -> Solv.Error err
    where result = parse parseCmdGenResponse "" stg


checkSatResponse :: String -> SatResult
checkSatResponse stg =
    case result of
        Left err ->  Solv.SUError $ show  err
        Right cmdRep -> case cmdRep of
                            S.Sat -> Solv.Sat
                            S.Unsat -> Solv.Unsat
                            S.Unknown -> Solv.Unknown
    where result = parse parseCheckSatResponse "" stg



getValueResponse :: String -> GValResult
getValueResponse stg = GVUError  $ stg ++ "\n" ++ tree
    where result = parse parseGetValueResponse "" stg
          tree =  getVR' result


getVR' :: Show a => Either a [ValuationPair] -> String
getVR' result =
  case result of
    Left err -> show err
    Right vals -> intercalate "\n" (fmap (showValuationPair 0) vals)




getFun :: ValuationPair -> Maybe GValResult
getFun vp = getFun' name value
            where name = getFunName vp
                  value = getFunResultInt vp


getFun' :: Maybe String -> Maybe Integer -> Maybe GValResult
getFun' (Just name) (Just res) = Just $  Fun name res



{- | Retrives the value of a variable.
     Works with:
     - Z3.
-}
getVar :: ValuationPair ->  Maybe GValResult
getVar vp = getVar' name value
            where name = getVarName vp
                  value = getVarValue vp

-- auxiliar function to getVar
getVar' :: Maybe String -> Maybe Integer -> Maybe GValResult
getVar' Nothing _ = Nothing
getVar' _ Nothing = Nothing
getVar' (Just name) (Just value) = Just $ Var name $ VInt value




{- | Retrives the value of an array.
     Works with:
      - Z3.
-}
getArray :: ValuationPair -> Maybe GValResult
getArray vp =
    case res of
         Just arr -> Just $ VArrays arr
         Nothing -> Nothing
    
    where name = arrayName vp
          posInt = arrayIntPos vp
          posVar = arrayVarPos vp
          val = arrayVal vp
          res = getArray' name posInt posVar val


-- Auxiliar function to getArray.
getArray' :: Maybe String
          -> Maybe Integer
          -> Maybe String
          -> Maybe Integer
          -> Maybe Arrays
getArray' (Just name) (Just pos) Nothing (Just val) =
    Just $ singleton name $ MI $ singleton pos val
getArray' (Just name) Nothing (Just pos) (Just val) =
    Just $ singleton name $ MS $ singleton pos val
getArray' _ _ _ _ = Nothing




-- Auxiliar functions to get values from constructed ast.



-- Auxliar functions to work with arrays.
{- | Retrives the name of the array.
     Works with:
      - Z3.
-}

arrayName :: ValuationPair -> Maybe String
arrayName = symbol
        <=< qIdentifier
        <=< termQualIdentifier
        <=< fstValArray
        <=< sndTermQualIdentierT
        <=< fstTerm
        where fstValArray = (\x -> Just $ head x)

{-| Retrives the position of the array if it is an Integer.
    Works with:
    - Z3.
-}
arrayIntPos :: ValuationPair -> Maybe Integer
arrayIntPos = numeral
          <=< getTermSpecConstant
          <=< sndValArray
          <=< sndTermQualIdentierT
          <=< fstTerm
          where sndValArray = (\x -> Just $ x !! 1)

{-| Retrives the position of the array if it is a String.
    Works with:
    - Z3.
-}
arrayVarPos :: ValuationPair -> Maybe String
arrayVarPos = symbol
          <=< qIdentifier
          <=< termQualIdentifier
          <=< sndValArray
          <=< sndTermQualIdentierT
          <=< fstTerm
          where sndValArray = (\x -> Just $ x !! 1)


{-| Retrives the value of an array.
    Works with:
    - Z3.
-}
arrayVal :: ValuationPair -> Maybe Integer
arrayVal = numeral <=< getTermSpecConstant <=< sndTerm



{- | Retrive the name of a variable.
     Works with:
     - Z3.

-}

getVarName :: ValuationPair -> Maybe String
getVarName = symbol <=< qIdentifier <=< termQualIdentifier <=< fstTerm


{- | Retrive the variable of a variable.
     Works with:
     - Z3.

-}
getVarValue :: ValuationPair -> Maybe Integer
getVarValue = numeral <=< getTermSpecConstant <=< sndTerm



{- | Retrives the name of a function.
     Works with:
     - Z3
-}
getFunName :: ValuationPair -> Maybe String
getFunName = symbol <=< qIdentifier <=< fstTermQualIdentierT <=< fstTerm



{- | Retrives the result of a function if it is a Integer.
     Works with:
     - Z3.
-}
getFunResultInt :: ValuationPair -> Maybe Integer
getFunResultInt = numeral <=< getTermSpecConstant <=< sndTerm



-- Auxiliar functions to get specific value from ast.


-- | Returns the first term of a valuation pair.
fstTerm :: ValuationPair -> Maybe Term
fstTerm (ValuationPair a _) = Just  a


-- | Returns the second term of a valuation pair.
sndTerm :: ValuationPair -> Maybe Term
sndTerm (ValuationPair _ b) = Just b


-- | Returns the list of terms from TermQualIdeintifierT
sndTermQualIdentierT :: Term -> Maybe [Term]
sndTermQualIdentierT (TermQualIdentifierT _ ts) = Just ts
sndTermQualIdentierT _ = Nothing

fstTermQualIdentierT :: Term -> Maybe QualIdentifier
fstTermQualIdentierT (TermQualIdentifierT qi _) = Just qi
fstTermQualIdentierT _ = Nothing


termQualIdentifier :: Term -> Maybe QualIdentifier
termQualIdentifier (TermQualIdentifier a) = Just a
termQualIdentifier _ = Nothing


getTermSpecConstant :: Term -> Maybe SpecConstant
getTermSpecConstant (TermSpecConstant spc) = Just spc
getTermSpecConstant _ = Nothing


qIdentifier :: QualIdentifier -> Maybe Identifier
qIdentifier (QIdentifier a) = Just a
qIdentifier _ = Nothing

symbol :: Identifier -> Maybe String
symbol (ISymbol s)  = Just s
symbol _ = Nothing


numeral :: SpecConstant -> Maybe Integer
numeral (SpecConstantNumeral n) = Just n
numeral _ = Nothing
