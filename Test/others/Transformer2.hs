module Transformer2 where

import Hsmtlib.Parsers.Syntax as St
import Data.List

imports :: String
imports = "import Hsmtlib\nimport Hsmtlib.Solver\nimport SMTLib2\n\n"

generateProgram :: String -> String -> String  -> Source ->  String
generateProgram solver mode logic src = imports ++ func ++ startSolver ++ fbody
    where  startSolver = "\tsolver <- startSolver " ++ solver ++ " " ++ mode ++ " " ++ logic ++ " Nothing Nothing"
           func = "main :: IO ()\nmain = do\n"
           body = toSMTLib2 "solver" src
           fbody = concat $ fmap (\x -> "\n\t" ++ x) body
toSMTLib2 :: String -> Source ->  [String]
toSMTLib2 solver = fmap (toSMTLib2' solver)

toSMTLib2' :: String ->  St.Command -> String
toSMTLib2' solver (SetLogic logic) = 
    "setLogic " ++ solver ++ " (N "++ show logic ++" ) >>=print"
toSMTLib2' solver (SetOption option) =
    "setOption " ++ solver ++  "( " ++ tOption option ++ ") >>=print"
toSMTLib2' solver (SetInfo attr) = 
    "setInfo " ++ solver ++ "(" ++ tAttr attr ++ ") >>=print"
toSMTLib2' solver (DeclareSort str n) = 
    "declareType " ++ solver ++ "( N " ++ show str ++" ) ( " ++ show n ++ " ) >>=print"  
toSMTLib2' solver (DefineSort str strs sort) = 
    "defineType  " ++ solver ++  " ( N " ++ show str ++ " ) ++ ( " ++ tNs strs ++ ") ( " ++  tSort sort ++ ") >>=print "
toSMTLib2' solver (DeclareFun str sorts sort ) =
    "declareFun " ++ solver ++ "(N "++ show str ++") [ " ++ intercalate "," (fmap tSort sorts) ++ "] ("++ tSort sort ++ ") >>=print "
toSMTLib2' solver (DefineFun str svs sort term) =
    "defineFun " ++ solver ++ "(N " ++ show str ++ ") [" ++ intercalate "," (fmap tSortedVar svs) ++ "] (" ++ tSort sort ++ ") ( "++  tTerm term ++ " ) >>=print"
toSMTLib2' solver (Push n) = "push >>=print " ++ solver ++ " " ++ show n
toSMTLib2' solver (Pop n) = "pop >>=print " ++ solver  ++ " " ++ show n
toSMTLib2' solver (Assert term) = "assert solver (" ++ tTerm term ++ ") >>=print"
toSMTLib2' solver CheckSat = "checkSat " ++  solver ++ ">>= print"
toSMTLib2' solver GetAssertions = "getAssertions >>=print " ++ solver
toSMTLib2' solver GetProof = "getProof >>=print " ++ solver
toSMTLib2' solver GetUnsatCore = "getUnsatCore >>=print " ++ solver
toSMTLib2' solver (GetValue terms) = "getValue >>=print " ++ solver ++ " [" ++ intercalate "," (fmap tTerm terms) ++ "] >>=print"
toSMTLib2' _ GetAssignment = "Missing GetAssignment >>=print"
toSMTLib2' solver (GetOption str) = "getOption " ++  solver ++ " (N " ++ show str ++ ") >>=print"
toSMTLib2' solver (GetInfo flag) = "getInfo " ++  solver ++ " (" ++ tInfo flag ++ ") >>=print"
toSMTLib2' solver Exit = "exit " ++ solver ++ ">>=print"


tNs :: [String] -> String
tNs ns =  "[ " ++ fun ++ " ]"
    where fun = intercalate ","  mapz
          mapz =  fmap (\x -> "(N " ++ show x ++ ")")  ns

tSort :: Sort -> String
tSort (SortId (ISymbol s)) = "TVar (N " ++ show s ++ ")"
tSort (SortId  iden) = "TApp (" ++ tIdent iden ++ ") [] " 
tSort (SortIdentifiers iden sorts) = "TApp (" ++ tIdent iden ++ ") [" ++ intercalate "," (fmap tSort sorts) ++ "]"

tIdent :: Identifier ->  String
tIdent (ISymbol symb) = "I (N " ++ show symb ++ ") []" 
tIdent (I_Symbol symb nums) = "I  (N " ++ show symb ++") (" ++ " [ " ++ intercalate "," (fmap show nums) ++ "])" 


tOption :: Option -> String
tOption (PrintSucess b) = "OptPrintSuccess " ++ show b
tOption (ExpandDefinitions b) = "OptExpandDefinitions " ++ show b
tOption (InteractiveMode b) = "OptInteractiveMode " ++ show b
tOption (ProduceProofs b) = "OptProduceProofs " ++ show b
tOption (ProduceUnsatCores b) = "OptProduceUnsatCores " ++ show b
tOption (ProduceModels b) = "OptProduceModels " ++ show b 
tOption (ProduceAssignments b) = "OptProduceAssignments " ++ show b
tOption (RegularOutputChannel str) = "OptRegularOutputChannel " ++ show str
tOption (DiagnosticOutputChannel str) = "OptDiagnosticOutputChannel " ++ show str
tOption (RandomSeed n) = "OptRandomSeed " ++ show n
tOption (Verbosity n) = "OptVerbosity "  ++ show n
tOption (OptionAttr attr) = "OptAttr " ++ "( " ++ tAttr attr ++ " )"


tAttr :: Attribute -> String
tAttr (Attribute str) = "Attr " ++ "(N " ++ show str ++ ") Nothing"  
tAttr (AttributeVal str val) = "Attr "  ++ "(N " ++ show str ++ ") Nothing"



tSpecConst  :: SpecConstant -> String
tSpecConst (SpecConstantNumeral n) = "LitNum " ++ show n
tSpecConst (SpecConstantDecimal n) = "LitFrac " ++  n 
tSpecConst (SpecConstantHexadecimal str) = "LitStr " ++ show str
tSpecConst (SpecConstantBinary str) = "LitStr " ++  show str
tSpecConst (SpecConstantString str) = "LitStr " ++ str

tSortedVar :: SortedVar -> String
tSortedVar (SV str sort) = "Bind (N " ++ show str ++ ") ( " ++ tSort sort ++ " ) "



tTerm :: Term -> String
tTerm (TermSpecConstant spc) = "Lit ( " ++ tSpecConst spc ++ " ) "
tTerm (TermQualIdentifier iden) = " App  ( " ++ qIdent ++ ") ( " ++ sorts ++ " ) []"
                            where (qIdent, sorts) = tQIdent iden
tTerm (TermQualIdentifierT iden terms) =" App ( " ++ qIdent ++ ") ( " ++ sorts ++ " ) [ " ++  intercalate "," (fmap tTerm terms) ++ " ]"
                            where (qIdent, sorts) = tQIdent iden
tTerm (TermLet vbs term) =  "Let [" ++ intercalate "," (fmap tVbs vbs) ++ "] ( " ++ tTerm term ++ " ) "
tTerm (TermForall svs term) = 
    " Quant Forall [" ++ intercalate "," (fmap tSortedVar svs) ++ " ] (" ++  tTerm term ++ ")"
tTerm (TermExists svs term) = 
    " Quant Exists [" ++ intercalate "," (fmap tSortedVar svs) ++ " ] (" ++  tTerm term ++ ")"
tTerm (TermAnnot term attrs) = "Annot (" ++ tTerm term ++ ") [ " ++ intercalate "," (fmap tAttr attrs) ++ " ]"

tVbs :: VarBinding ->  String
tVbs (VB str term) = "Defn (N " ++ show str ++ ") ( "++ tTerm term ++ " ) "


tQIdent :: QualIdentifier -> (String, String)
tQIdent (QIdentifier iden ) = (tIdent iden, "Nothing")
tQIdent (QIdentifierAs iden sort) = (tIdent iden, "(Just ( " ++ tSort sort ++ " ) )")


tInfo :: InfoFlags -> String
tInfo ErrorBehavior = "InfoErrorBehavior " 
tInfo Name = "InfoName "
tInfo Authors = "InfoAuthors "
tInfo Version = "InfoVersion "
tInfo Status = "InfoStatus "
tInfo ReasonUnknown = "InfoReasonUnknown "
tInfo AllStatistics = "InfoAllStatistics "
tInfo (InfoFlags str) = "InfoAttr ( Attr (N " ++ show str ++") Nothing)" 
