module Hsmtlib.Transformer.Transformer2 where

import Hsmtlib.Parsers.Syntax as St
import Data.List

imports :: String
imports = "import Hsmtlib\nimport Hsmtlib.Solver\nimport SMTLib2\n\n"

generateProgram :: String -> String  -> Source ->  String
generateProgram solver logic src = imports ++ func ++ startSolver ++ fbody
    where  startSolver = "\tsolver <- startSolver " ++ solver ++ " " ++ logic ++ " Nothing Nothing Nothing"
           func = "main :: IO ()\nmain = do\n"
           body = toSMTLib2 "solver" src
           fbody = concat $ fmap (\x -> "\n\t" ++ x) body


toSMTLib2 :: String -> Source ->  [String]
toSMTLib2 solver = fmap (toSMTLib2' solver)

toSMTLib2' :: String ->  St.Command -> String
toSMTLib2' solver (SetLogic logic) = 
    "setLogic " ++ solver ++ " (N "++ show logic ++" )"
toSMTLib2' solver (SetOption option) =
    "setOption " ++ solver ++  "( " ++ tOption option ++ ")"
toSMTLib2' solver (SetInfo attr) = 
    "setInfo " ++ solver ++ "( " ++ tAttr attr ++ " )"
toSMTLib2' solver (DeclareSort str n) = 
    "declareType " ++ solver ++ "( N " ++ show str ++" ) ( " ++ show n ++ " )"  
toSMTLib2' solver (DefineSort str strs sort) = 
    "defineType  " ++ solver ++  " ( N " ++  show str ++ " ) ++ ( " ++ tNs strs ++ ") ( " ++  tSort sort ++ ") "
toSMTLib2' solver (DeclareFun str sorts sort ) =
    "declareFun " ++ solver ++ "(N "++ show str ++") [ " ++ intercalate "," (fmap tSort sorts) ++ "] ("++ tSort sort ++ ") "
toSMTLib2' solver (DefineFun str svs sort term) =
    "defineFun " ++ solver ++ "(N " ++ show str ++ ") [" ++ intercalate "," (fmap tSortedVar svs) ++ "] (" ++ tSort sort ++ ") ( "++  tTerm term ++ " )"
toSMTLib2' solver (Push n) = "push " ++ solver ++ " " ++ show n
toSMTLib2' solver (Pop n) = "pop " ++ solver  ++ " " ++ show n
toSMTLib2' solver (Assert term) = "assert solver (" ++ tTerm term ++ ")"
toSMTLib2' solver CheckSat = "checkSat " ++  solver
toSMTLib2' solver GetAssertions = "getAssertions " ++ solver
toSMTLib2' solver GetProof = "getProof " ++ solver
toSMTLib2' solver GetUnsatCore = "getUnsatCore " ++ solver
toSMTLib2' solver (GetValue terms) = "getValue " ++ solver ++ " [" ++ intercalate "," (fmap tTerm terms) ++ "]"
toSMTLib2' _ GetAssignment = "Missing GetAssignment"
toSMTLib2' solver (GetOption str) = "getOption " ++  solver ++ " (N " ++ show str ++ ")"
toSMTLib2' solver (GetInfo flag) = "getInfo " ++  solver ++ " (" ++ tInfo flag ++ ")"
toSMTLib2' solver Exit = "exit " ++ solver


tNs :: [String] -> String
tNs ns =  "[ " ++ fun ++ " ]"
    where fun = intercalate ","  mapz
          mapz =  fmap (\x -> "(N " ++ show x ++ ")")  ns

tSort :: Sort -> String
tSort (SortId (ISymbol s)) = "TVar (N " ++ show s ++ ")"
tSort (SortId  iden) = "TApp (" ++ tIdent iden ++ ") [] " 
tSort (SortIdentifiers iden sorts) = "TApp (" ++ tIdent iden ++ " [" ++ intercalate "," (fmap tSort sorts) ++ "]"

tIdent :: Identifier ->  String
tIdent (ISymbol symb) = "I (N " ++ show symb ++ ") []" 
tIdent (I_Symbol symb nums) = "I  (N " ++ show symb ++") (" ++ " [ " ++ intercalate "," (fmap read nums) ++ "]" 


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
tAttr (Attribute str) = "Attr " ++ "( N " ++ show str ++ " ) Nothing"  
tAttr (AttributeVal str val) = "Attr "  ++ "( N " ++ show str ++ " ) ( Just ( " ++ tAttrVal val ++ " )"


tAttrVal :: AttrValue -> String
tAttrVal (AttrValueConstant spc) = "Lit ( " ++ tSpecConst spc ++ " )"
tAttrVal (AttrValueSymbol str) = "Lit (LitStr " ++ str ++ " )"
tAttrVal (AttrValueSexpr _) = "Lit (LitNum( -99999999999 What?) "  


tSpecConst  :: SpecConstant -> String
tSpecConst (SpecConstantNumeral n) = "LitNum " ++ show n
tSpecConst (SpecConstantDecimal n) = "LitStr " ++ show n
tSpecConst (SpecConstantHexadecimal str) = "LitStr " ++ show str
tSpecConst (SpecConstantBinary str) = "LitStr " ++ show str
tSpecConst (SpecConstantString str) = "LitStr " ++ show str

tSortedVar :: SortedVar -> String
tSortedVar (SV str sort) = "Bind (N " ++  str ++ ") ( " ++ tSort sort ++ " ) "



tTerm :: Term -> String
tTerm (TermSpecConstant spc) = "Lit ( " ++ tSpecConst spc ++ " ) "
tTerm (TermQualIdentifier iden) = " App " ++ " ( " ++ qIdent ++ ") ( " ++ sorts ++ " ) []"
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
tQIdent (QIdentifierAs iden sort) = (tIdent iden, "(Just ( " ++ tSort sort ++ " ) ")


tInfo :: InfoFlags -> String
tInfo ErrorBehavior = "InfoErrorBehavior " 
tInfo Name = "InfoName "
tInfo Authors = "InfoAuthors "
tInfo Version = "InfoVersion "
tInfo Status = "InfoStatus "
tInfo ReasonUnknown = "InfoReasonUnknown "
tInfo AllStatistics = "InfoAllStatistics "
tInfo (InfoFlags str) = "InfoAttr ( Attr (N " ++ show str ++") Nothing)" 


{-



toSMTLib2' solver (Push n) = push solver (toInteger n)
toSMTLib2' solver (Pop n) = pop solver (toInteger n)
toSMTLib2' solver (Assert term) = assert solver (tTerm term)
toSMTLib2' solver CheckSat = checkSat solver
toSMTLib2' solver GetAssertions = getAssertions solver
toSMTLib2' solver GetProof = getProof solver
toSMTLib2' solver GetUnsatCore = getUnsatCore solver
toSMTLib2' solver (GetValue terms) = getValue solver (fmap tTerm terms)
toSMTLib2' _ GetAssignment = return $ ComError "Missing GetAssignment"
toSMTLib2' solver (GetOption str) = getOption solver (N str)
toSMTLib2' solver (GetInfo flag) = getInfo solver (tInfo flag)
toSMTLib2' solver Exit = exit solver


tInfo :: InfoFlags -> InfoFlag
tInfo ErrorBehavior = InfoErrorBehavior
tInfo Name = InfoName
tInfo Authors = InfoAuthors
tInfo Version = InfoVersion
tInfo Status = InfoStatus
tInfo ReasonUnknown = InfoReasonUnknown
tInfo AllStatistics = InfoAllStatistics
tInfo (InfoFlags str) = InfoAttr $ Attr (N str) Nothing


tNs :: [String] -> [Name]
tNs = fmap N

tSort :: Sort -> Type
tSort (SortId (ISymbol s)) = TVar (N s)
tSort (SortId  iden) = TApp (tIdent iden) [] 
tSort (SortIdentifiers iden sorts) = TApp (tIdent iden) (fmap tSort sorts)


tIdent :: Identifier ->  Ident
tIdent (ISymbol symb) = I (N symb) []
tIdent (I_Symbol symb nums) = I  (N symb) (fmap read nums) 
 


tOption :: St.Option -> SM.Option
tOption (PrintSucess b) = OptPrintSuccess b
tOption (ExpandDefinitions b) = OptExpandDefinitions b
tOption (InteractiveMode b) = OptInteractiveMode b
tOption (ProduceProofs b) = OptProduceProofs b
tOption (ProduceUnsatCores b) = OptProduceUnsatCores b
tOption (ProduceModels b) = OptProduceModels b 
tOption (ProduceAssignments b) = OptProduceAssignments b
tOption (RegularOutputChannel str) = OptRegularOutputChannel str
tOption (DiagnosticOutputChannel str) = OptDiagnosticOutputChannel str
tOption (RandomSeed n) = OptRandomSeed  $ toInteger n  
tOption (Verbosity n) = OptVerbosity $ toInteger n
tOption (OptionAttr attr) = OptAttr $ tAttr attr



tAttrVal :: AttrValue -> AttrVal
tAttrVal (AttrValueConstant spc) = Lit $ tSpecConst spc
tAttrVal (AttrValueSymbol str) = Lit $ LitStr str
tAttrVal (AttrValueSexpr _) =  Lit(LitNum (-99999999999999999999999999999999))




tSortedVar :: SortedVar -> Binder
tSortedVar (SV str sort) = Bind (N str) (tSort sort)
    

tTerm :: Term -> Expr
tTerm (TermSpecConstant spc) = Lit $ tSpecConst spc
tTerm (TermQualIdentifier iden) = App qIdent sorts []
                            where (qIdent, sorts) = tQIdent iden
tTerm (TermQualIdentifierT iden terms) = App qIdent sorts (fmap tTerm terms)
                            where (qIdent, sorts) = tQIdent iden
tTerm (TermLet vbs term) =  Let (fmap tVbs vbs) (tTerm term)
tTerm (TermForall svs term) = 
    Quant Forall (fmap tSortedVar svs) (tTerm term)
tTerm (TermExists svs term) = 
    Quant Exists (fmap tSortedVar svs) (tTerm term)
tTerm (TermAnnot term attrs) = Annot (tTerm term) (fmap tAttr attrs)

tVbs :: VarBinding ->  Defn
tVbs (VB str term) = Defn (N str) (tTerm term)


tQIdent :: QualIdentifier -> (Ident, Maybe Type)
tQIdent (QIdentifier iden ) = (tIdent iden, Nothing)
tQIdent (QIdentifierAs iden sort) = (tIdent iden, Just $ tSort sort)
-}