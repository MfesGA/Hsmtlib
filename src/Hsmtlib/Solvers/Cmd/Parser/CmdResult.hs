module  Hsmtlib.Solvers.Cmd.Parser.CmdResult where

import           Control.Applicative                    hiding (empty)
import           Control.Monad
import           Data.Map
import           Hsmtlib.Solver                         as Solv
import           Hsmtlib.Solvers.Cmd.Parser.Parsers     hiding (numeral, symbol)
import           Hsmtlib.Solvers.Cmd.Parser.Syntax      as S
import           Hsmtlib.Solvers.Cmd.Parser.Visualizers
import           Text.ParserCombinators.Parsec.Prim     (parse)

genResponse :: String -> Result
genResponse stg =
    case result of
        Left err -> Solv.UError $ show err
        Right cmdRep -> case cmdRep of
                            CmdGenResponse S.Success -> Solv.Success
                            CmdGenResponse S.Unsupported -> Solv.Unsupported
                            CmdGenResponse (S.Error err) -> Solv.Error err
    where result = parse parseCmdGenResponse "" stg


checkSatResponse :: String -> Result
checkSatResponse stg =
    case result of
        Left err -> Solv.UError $ show err
        Right cmdRep -> case cmdRep of
                            S.Sat -> Solv.Sat
                            S.Unsat -> Solv.Unsat
                            S.Unknown -> Solv.Unknown
    where result = parse parseCheckSatResponse "" stg





cenasFold :: [ValuationPair] -> [Maybe Result]
cenasFold vp =


cenas :: [ValuationPair] -> Maybe Result
cenas (x:y:ts)= (getArraysResult x y <|>
                getFunctionResult x <|>
                getVar x <|>
                getFunResult y
                get







tryGetResult :: [ValuationPair] -> Maybe Result
tryGetResult vp = getArraysResult vp <|> (Just (Several (tryOthers vp)))




tryOthers  :: [ValuationPair] -> [Result]
tryOthers [] = []
tryOthers (x:xs) = noMaybe (tryOthers' x) : tryOthers xs


tryOthers' :: ValuationPair -> Maybe Result
tryOthers' vp = getFunctionResult vp <|> getVar vp


noMaybe :: Maybe Result -> Result
noMaybe Nothing = UError "Unknown"
noMaybe (Just r) = r

-- get Value Response
getValueResponse :: String -> Result
getValueResponse stg =  gVR result
    where result = parse parseGetValueResponse "" stg





gVR :: Show a => Either a [ValuationPair] -> Result
gVR (Left x) = UError $ show x
gVR (Right k) = case tryGetResult k of
                  Nothing  -> UError $ show k
                  Just x -> x


stringVP :: [ValuationPair] -> String
stringVP [] = ""
stringVP (x:xs) = showValuationPair 0 x ++ "\n" ++ stringVP xs

--gVR (Right k) = show $ foldArrays k



getArrays :: Maybe Arrays -> Maybe Result
getArrays Nothing = Nothing
getArrays (Just x) = Just $ VArrays x


foldArrays :: [ValuationPair] -> Maybe Arrays
foldArrays [] = Just empty
foldArrays (_:[]) = Nothing
foldArrays (x:y:ts) = joinArrays (vp2Array x y) (foldArrays ts)


getArraysResult :: [ValuationPair] -> Maybe Result
getArraysResult = getArrays.foldArrays

getFunctionResult :: ValuationPair  -> Maybe Result
getFunctionResult = funRes.getFunRes



funRes :: Maybe Integer -> Maybe Result
funRes Nothing = Nothing
funRes (Just v) = Just $ Res $ VInt v

getFunRes :: ValuationPair -> Maybe Integer
getFunRes =  numeral <=< getTermSpecConstant <=< sndTerm

getVar :: ValuationPair ->  Maybe Result
getVar vp = getVar' name value
            where name = getVarName vp
                  value = getVarValue vp

getVar' :: Maybe String -> Maybe Integer -> Maybe Result
getVar' Nothing _ = Nothing
getVar' _ Nothing = Nothing
getVar' (Just name) (Just value) = Just $ Var name $ VInt value


getVarName :: ValuationPair -> Maybe String
getVarName = symbol <=< qIdentifier <=< termQualIdentifier<=< fstTerm

getVarValue :: ValuationPair -> Maybe Integer
getVarValue = numeral <=< getTermSpecConstant <=<sndTerm

{- Joins the outer arrays and the inner arrays if any of the arrays
 have the same name -}
joinArrays :: Maybe Arrays -> Maybe Arrays -> Maybe Arrays
joinArrays Nothing _ = Nothing
joinArrays _ Nothing = Nothing
joinArrays (Just arr1) (Just arr2) = Just $ unionWith union arr1 arr2



getArraysResult :: ValuationPair -> ValuationPair -> Mayb Result
getArraysResult vp1 vp2 = case vp2Array of
                          Just x -> VArrays x
                          Nothing -> Nothing


vp2Array :: ValuationPair -> ValuationPair -> Maybe Arrays
vp2Array name array = vp2Array' arrayName arrayIndex arrayValue
  where arrayName = getArrayName name
        arrayIndex = getIndexVP array
        arrayValue = getValueVP array


vp2Array' :: Maybe String -> Maybe Integer -> Maybe Integer -> Maybe Arrays
vp2Array' Nothing _ _ = Nothing
vp2Array' _ Nothing _ = Nothing
vp2Array' _ _ Nothing = Nothing
vp2Array' (Just name) (Just index) (Just val) = Just arrays
        where array = insert index (VInt val) empty
              arrays = insert name array empty

-- Get the Index of the Array from a Valuation Pair.

getIndexVP :: ValuationPair -> Maybe Integer
--getIndexVP vp = fstTerm vp >>=  getTermSpecConstant >>= numeral
getIndexVP = numeral <=< getTermSpecConstant <=< fstTerm

-- Get the Value of the Array from a Valuation Pair.
getValueVP :: ValuationPair -> Maybe Integer
getValueVP = numeral <=< getTermSpecConstant <=< sndTerm


-- Get the name of the Array from a Valuation Pair.
getArrayName :: ValuationPair -> Maybe String
getArrayName = symbol <=< qIdentifier <=< termQualIdentifier <=< fstTerm

fstTerm :: ValuationPair -> Maybe Term
fstTerm (ValuationPair a _) = Just  a

sndTerm :: ValuationPair -> Maybe Term
sndTerm (ValuationPair _ b) = Just b


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
