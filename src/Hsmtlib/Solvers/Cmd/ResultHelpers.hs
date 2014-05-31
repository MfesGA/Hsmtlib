module Hsmtlib.Solvers.Cmd.ResultHelpers where

import           Control.Applicative                   ((<$>), (<|>))
import           Control.Monad
import           Hsmtlib.Parsers.Syntax        as S
import           Hsmtlib.Solver                        as Solv
import           Data.Map                           as M


{- | Retrives the value of a BitVector.
     Works with:
     - Z3.
-}
getBitVec :: ValuationPair -> Maybe GValResult
getBitVec vp = getBitVec' name vhex
               where name = getVarName vp
                     vhex = getHexVal vp


-- auxiliar function to getBitVec
getBitVec' :: Maybe String -> Maybe String -> Maybe GValResult
getBitVec' (Just name) (Just vhex) = Just $ Res name $ VHex vhex
getBitVec' _ _ = Nothing

{- | Retrives the value of a function.
     Works with:
     - Z3.
-}
getFun :: ValuationPair -> Maybe GValResult
getFun vp = getFun' name value
            where name = getFunName vp
                  value = getFunResult vp

-- auxiliar function to getFun
getFun' :: Maybe String -> Maybe Value -> Maybe GValResult
getFun' (Just name) (Just res) = Just $  Res name  res
getFun' _ _ = Nothing


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
getVar' (Just name) (Just value) = Just $ Res name $ VInt value




{- | Retrives the value of an array.
     Works with:
      - Z3.
-}
getArray :: ValuationPair -> Maybe GValResult
getArray vp =
  case isArr of
    Nothing -> Nothing
    Just _ -> case res of
                Just arr -> Just $ VArrays arr
                Nothing -> Nothing

    where name = arrayName vp
          posInt = arrayIntPos vp
          posVar = arrayVarPos vp
          val = arrayVal vp
          isArr = isArray vp
          res = getArray' name posInt posVar val


-- Auxiliar function to getArray.
getArray' :: Maybe String
          -> Maybe Integer
          -> Maybe String
          -> Maybe Integer
          -> Maybe Arrays
getArray' (Just name) (Just pos) Nothing (Just val) =
    Just $ singleton name $  singleton (show pos) val
getArray' (Just name) Nothing (Just pos) (Just val) =
    Just $ singleton name $ singleton pos val
getArray' _ _ _ _ = Nothing




-- Auxiliar functions to get values from constructed ast.





isArray :: ValuationPair -> Maybe Bool
isArray = isArray'
      <=< symbol
      <=< qIdentifier
      <=< fstTermQualIdentierT
      <=< fstTerm


{- | Verifies if the string correspond to a certain notation that indicates
     that is an Array.
     For example Z3 would have the keyword select therefor it's an array.
     If it isn't an array then returns nothing

-}
isArray' :: String -> Maybe Bool
isArray' "select" = Just True
isArray' _ = Nothing


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
        where fstValArray x = Just $ head x

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
          where sndValArray x = Just $ x !! 1

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
          where sndValArray x = Just $ x !! 1


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


getFunResult :: ValuationPair -> Maybe Value
getFunResult vp = getFunResultBool vp <|> getFunResultInt vp

getFunResultBool :: ValuationPair -> Maybe Value
getFunResultBool = VBool <#> toBool <=< getFunResultBool'


getFunResultInt :: ValuationPair -> Maybe Value
getFunResultInt = VInt <#> getFunResultInt'


{-
   #########################################################################
   #                                                                       #
   #                           Auxiliar functions to acess Syntax          #
   #                                                                       #
   #########################################################################
-}


(<#>):: Functor m => (b -> c) -> (a -> m b) -> a -> m c
(f <#> m) x  = f <$> m x

toBool :: String -> Maybe Bool
toBool "true" = Just True
toBool "false" = Just False
toBool _ = Nothing

getFunResultInt' :: ValuationPair -> Maybe Integer
getFunResultInt' = numeral <=< getTermSpecConstant <=< sndTerm


getFunResultBool' :: ValuationPair -> Maybe String
getFunResultBool' = symbol <=< qIdentifier <=< termQualIdentifier <=< sndTerm


{- | Retrives the result of a bitvector.
     Works with:
     - Z3
-}
getHexVal :: ValuationPair -> Maybe String
getHexVal = hex <=< getTermSpecConstant <=< fstTerm


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

hex :: SpecConstant -> Maybe String
hex (SpecConstantHexadecimal shex) = Just shex
hex  _ = Nothing
