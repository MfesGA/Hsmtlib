module Hsmtlib.Solvers.Cmd.Parser.Visualizers where

import           Hsmtlib.Solvers.Cmd.Parser.Syntax  as S
import           Data.List




spaces' :: Int -> Int -> String
spaces' n limit | n == limit = ""
                | otherwise = " " ++  spaces' (n+1) limit

spaces :: Int -> String
spaces = spaces' 0

branch' :: Int -> String
branch' nsp = "\n" ++ spaces nsp++"|\n" ++ spaces nsp ++ "|- "

branchAr' :: Int -> String
branchAr' nsp = "\n" ++ spaces nsp ++"\n" ++ spaces (nsp+1) ++ "["

fbranch :: Int  -> String -> String -> String
fbranch nsp mark fin = mark ++ branch' nsp ++ fin

branch ::  String -> (Int -> a -> String) -> Int -> a -> String
branch mark f nsp arg = mark ++ branch' nsp ++ f (nsp+3) arg


nLines :: String -> Int
nLines = length.lines

mLines :: String ->Int -> String
mLines _ 0 = ""
mLines sps n =  sps ++ "|\n" ++ mLines sps (n-1)

showValuationPair :: Int -> ValuationPair-> String
showValuationPair nsp (ValuationPair t1 t2) = tup "ValP" nsp t1 t2 showTerm showTerm

showSpecConstant :: SpecConstant -> String
showSpecConstant (SpecConstantNumeral int) = "Num " ++ show int
showSpecConstant (SpecConstantDecimal st) = "Dec " ++ st
showSpecConstant (SpecConstantHexadecimal st) = "Hex " ++ st
showSpecConstant (SpecConstantBinary it) = "Bin " ++  show it
showSpecConstant (SpecConstantString st) = "Str " ++ st


aTup :: String
     -> Int
     -> [a]
     -> (Int -> a -> String)
     -> String
aTup mark nsp arg f =
  mark ++ " -[ " ++ hd ++ arr ++ endSpace++"]"
  where
    hd =f (nsp+8) (head arg)
    bd = drop 1 arg
    arr = backSpace ++ intercalate backSpace (fmap (f (nsp+8)) bd)
    backSpace = "\n" ++ spaces (nsp+6) ++ ", "
    endSpace = "\n" ++ spaces (nsp+6)

dTup :: String
     -> Int
     -> String
     -> a
     -> (Int -> a -> String)
     -> String
dTup mark nsp st arg f =
  bMark ++ st ++ sndStr
  where bMark = mark ++ branch' nsp
        sndStr = branch' nsp  ++  f (nsp+3) arg

tup :: String
    -> Int
    -> a
    -> b
    -> (Int -> a -> String)
    -> (Int -> b -> String)
    -> String
tup mark nsp arg1 arg2 f1 f2 =
    bMark  ++  bf1 ++ bf2
    where bMark = mark ++ branch' nsp
          bf1 = f1 (nsp+3) arg1 ++ "\n" ++ spaces nsp ++"|- "
          bf2 = f2 (nsp+3) arg2


tupArray :: String
         -> Int
         -> a
         -> [b]
         -> (Int -> a -> String)
         -> (Int -> b -> String)
         -> String
tupArray mark nsp arg1 arg2  f1 f2 =
    mark ++ fstf ++ sndf
    where fstf = "\n" ++spaces nsp ++ "|- "  ++  f1 (nsp+3) arg1
          sndf ="\n" ++ spaces nsp ++ "|-[ "  ++  hd ++ strs ++ endSpace
          hd = f2 (nsp+5) (head arg2) ++ backSpace
          bd = drop 1 arg2
          strs = intercalate backSpace (fmap (f2 (nsp+5)) bd)
          backSpace = "\n" ++ spaces (nsp+2) ++ ", "
          endSpace =  "\n" ++ spaces (nsp+2) ++ "]"

showQualIdentifier :: Int -> QualIdentifier -> String
showQualIdentifier nsp (QIdentifier qid) = branch "QI" showIdentifier nsp qid
showQualIdentifier nsp (QIdentifierAs iden srt) =
    tup "QIAS" nsp iden srt showIdentifier showSort



showSort :: Int -> Sort -> String
showSort nsp (SortId iden) = branch "SID" showIdentifier nsp iden
showSort nsp (SortIdentifiers iden sorts) =
    tupArray "SIden" nsp iden sorts showIdentifier showSort



showIdentifier :: Int ->  Identifier -> String
showIdentifier _ (ISymbol st) = "IS " ++ st
showIdentifier nsp (I_Symbol str1  str2) =
    "I_S" ++ fstr ++ sstr
    where brc = "\n" ++ spaces nsp ++ "|\n" ++ spaces nsp
          fstr = brc ++  "|- "  ++ str1
          sstr = brc ++ "|- [ " ++hd ++ strs ++ end
          hd = head str2 ++ "\n"
          bd = drop 1 str2
          strs = intercalate "\n" (fmap ((spaces (nsp + 3) ++ ", ") ++) bd)
          end = "\n" ++ spaces (nsp+3) ++ "]"




showTerm ::Int -> Term -> String
showTerm nsp (TermSpecConstant sc) =
  fbranch nsp "TSC" $ showSpecConstant  sc
showTerm nsp (TermQualIdentifier qi) =
  branch "TQI" showQualIdentifier nsp qi
showTerm nsp (TermQualIdentifierT qi ts) =
  tupArray  "TQIT" nsp qi ts showQualIdentifier showTerm
showTerm nsp (TermLet vb t) =
  tupArray "TLt" nsp t vb showTerm showVarBinding
showTerm nsp (TermForall sv  t) =
  tupArray "TFA" nsp t sv showTerm showSortedVar
showTerm nsp (TermExists sv  t) =
  tupArray "TE" nsp t sv showTerm showSortedVar
showTerm nsp (TermAnnot t attrs) =
  tupArray  "TAN" nsp t attrs showTerm showAttribute





showVarBinding :: Int -> VarBinding -> String
showVarBinding  nsp (VB stg term) = dTup "VB" nsp stg term showTerm

showSortedVar :: Int -> SortedVar -> String
showSortedVar nsp  (SV stg st) = dTup "SV" nsp stg st showSort

showAttribute :: Int -> Attribute -> String
showAttribute nsp (Attribute st) = fbranch nsp "Attribute"  st
showAttribute nsp (AttributeVal stg av) =
  dTup "AttrVal" nsp stg av showAttrValue

showAttrValue :: Int -> AttrValue -> String
showAttrValue nsp (AttrValueConstant sc) =
  fbranch nsp "AVC" $ showSpecConstant sc
showAttrValue nsp (AttrValueSymbol st) =
  fbranch nsp "AVS" st
showAttrValue nsp (AttrValueSexpr sxpr) =
  aTup "AVSP" nsp  sxpr showSexpr


showSexpr :: Int ->  Sexpr -> String
showSexpr nsp (SexprSpecConstant sc) =
  fbranch nsp "SSC" $ showSpecConstant sc
showSexpr nsp (SexprSymbol st) =
  fbranch nsp "SSymb" st
showSexpr nsp (SexprKeyword st) =
  fbranch nsp "SK" st
showSexpr nsp (SexprSxp sexprs) =
  aTup "SSxp" nsp sexprs showSexpr