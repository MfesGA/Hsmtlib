module Hsmtlib.Transformer.Transformer where

import SMTLib2 as SM
import Hsmtlib.Parsers.Syntax as St
import Hsmtlib.Solver 




toSMTLib2 :: Solver -> Source ->  [IO Result]
toSMTLib2 solver = fmap (toSMTLib2' solver)

toSMTLib2' :: Solver ->  St.Command -> IO Result
toSMTLib2' solver (SetLogic logic) =  setLogic  solver (N logic)
toSMTLib2' solver (SetOption  option) = setOption solver (tOption option)
toSMTLib2' solver (SetInfo attr) = setInfo solver (tAttr attr)
toSMTLib2' solver (DeclareSort str n) = declareType solver (N str) (toInteger n)
toSMTLib2' solver (DefinesySort str strs sort) = 
    defineType solver (N str) (tNs strs) (tSort sort)
toSMTLib2' solver (DeclareFun str sorts sort ) =
    declareFun solver (N str) (fmap tSort sorts) (tSort sort)
toSMTLib2' solver (DefineFun str svs sort term) =
    defineFun solver (N str) (fmap tSortedVar svs) (tSort sort) (tTerm term)
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


tAttr :: Attribute -> Attr
tAttr (Attribute str) = Attr (N str) Nothing
tAttr (AttributeVal str val) = Attr (N str) (Just $ tAttrVal val)

tAttrVal :: AttrValue -> AttrVal
tAttrVal (AttrValueConstant spc) = Lit $ tSpecConst spc
tAttrVal (AttrValueSymbol str) = Lit $ LitStr str
tAttrVal (AttrValueSexpr _) =  Lit(LitNum (-99999999999999999999999999999999))

tSpecConst  :: SpecConstant -> Literal
tSpecConst (SpecConstantNumeral n) = LitNum n
tSpecConst (SpecConstantDecimal n) = LitStr n
tSpecConst (SpecConstantHexadecimal str) = LitStr str
tSpecConst (SpecConstantBinary str) = LitStr str
tSpecConst (SpecConstantString str) = LitStr str


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
