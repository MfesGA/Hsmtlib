module CmdResponse where


-- S-expressions
data SpecConstant = SpecConstantNumeral Integer
                  | SpecConstantDecimal String
                  | SpecContantHexadecimal String
                  | SpecContantBinary Integer
                  | SpecConstantString String
                  deriving (Show)

data Sexpr = SexprSpecConstant SpecConstant
           | SexprSymbol String
           | SexprKeyword String
           | SexprSxp [Sexpr]
           deriving (Show)


-- Identifiers
data Identifier =  ISymbol String | I_Symbol  String [String] deriving (Show)


-- Sorts
data Sort = SortId Identifier | SortIdentifiers Identifier [Sort]
            deriving (Show)

-- Attributes

data AttrValue = AttrValueConstant SpecConstant
               | AttrValueSymbol String
               | AttrValueSexpr [Sexpr]
               deriving (Show)

data Attribute = Attribute String
               | AttributeVal String AttrValue
               deriving (Show)

-- Terms

data QualIdentifier = QIdentifier Identifier
                    | QIdentifierAs Identifier Sort
                    deriving (Show)

data VarBinding = VB String Term deriving (Show)

data SortedVar = SV String Sort deriving (Show)

data Term = TermSpecConstant SpecConstant
          | TermQualIdentifier QualIdentifier
          | TermQualIdentifierT  QualIdentifier [Term]
          | TermLet [VarBinding] Term
          | TermForall [SortedVar] Term
          | TermExists [SortedVar] Term
          | TermAnnot Term [Attribute]
          deriving (Show)



-- Command Responses


-- Gen Response

data GenResponse =  Unsupported |  Success | Error String deriving (Show)


-- Error behavior
data ErrorBehavior = ImmediateExit | ContinuedExecution deriving (Show)


-- Reason unknown
data ReasonUnknown = Memout | Incomplete deriving (Show)


-- Status

data CheckSatResponse = Sat | Unsat | Unknown deriving (Show)



-- Info Response

type GetInfoResponse = [InfoResponse]




data InfoResponse  = ResponseErrorBehavior ErrorBehavior
                   | ResponseName String
                   | ResponseAuthors String
                   | ResponseVersion String
                   | ResponseReasonUnknown ReasonUnknown
                   | ResponseAttribute Attribute
                   deriving (Show)



-- Get Assertion Response

type GetAssertionsResponse = [Term]



-- Get Proof Response

type GetProofResponse = Sexpr


--Get Unsat Core Response
type GetUnsatCoreResponse = [String]


-- Get Valuation Pair
data ValuationPair = ValuationPair Term Term deriving (Show)

type GetValueResponse = [ValuationPair]


-- get Assignment Response

data TValuationPair = TValuationPair String Bool deriving (Show)

type GetAssignmentResponse = [TValuationPair]


-- Get Option Response
type GetOptionResponse = AttrValue


-- CmdResponse
data CmdResponse = CmdGenResponse GenResponse
                 | CmdGetInfoResponse GetInfoResponse
                 | CmdCheckSatResponse CheckSatResponse
                 | CmdGetAssertionResponse GetAssignmentResponse
                 | CmdGetProofResponse GetProofResponse
                 | CmdGetUnsatCoreResoponse GetUnsatCoreResponse
                 | CmdGetValueResponse GetValueResponse
                 | CmdGetAssigmnentResponse TValuationPair
                 | CmdGetOptionResponse GetOptionResponse
                 deriving (Show)



