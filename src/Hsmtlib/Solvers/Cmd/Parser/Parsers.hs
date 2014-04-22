 module Hsmtlib.Solvers.Cmd.Parser.Parsers where

import           Control.Applicative               as Ctr hiding ((<|>))
import           Control.Monad
import           Data.Functor.Identity
import           Hsmtlib.Solvers.Cmd.Parser.Syntax as CmdRsp
import           Text.Parsec.Prim                  as Prim
import           Text.ParserCombinators.Parsec     as Pc



(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) a b = (++) <$> a <*> b


--Auxiliar parsers
aspO :: ParsecT String u Identity Char
aspO = char '('

aspC :: ParsecT String u Identity Char
aspC = char ')'

aspUS :: ParsecT String u Identity Char
aspUS = char '_'

numeral :: ParsecT String u Identity String
numeral = many1 digit

decimal :: ParsecT String u Identity String
decimal = numeral <++>  dot <++> zeros <++> numeral

zeros :: ParsecT String u Identity String
zeros = Pc.many $ char '0'

dot :: ParsecT String u Identity String
dot = string "."

hexadecimal :: ParsecT String u Identity String
hexadecimal = string "#x" *> many1 hexDigit

binary :: ParsecT String u Identity String
binary = string "#b" *> many1 bin

bin :: ParsecT String u Identity Char
bin = char '0' <|> char '1'


str :: ParsecT String u Identity String
str = string "\"" <++> Pc.many (alphaNum <|> char ' ') <++> string "\""


symbol :: ParsecT String u Identity String
symbol = simpleSymbol <|> quotedSymbol

quotedSymbol :: ParsecT String u Identity String
quotedSymbol = char '|' <:> Pc.many alphaNum <++> string "|"

simpleSymbol :: ParsecT String u Identity String
simpleSymbol = (letter <|> spcSymb) <:>  sq
    where sq = Pc.many (alphaNum <|> spcSymb)

spcSymb :: ParsecT String u Identity Char
spcSymb = oneOf  "+-/*=%?!.$_~^&<>"


keyword :: ParsecT String u Identity String
keyword = char ':' <:> Pc.many (alphaNum<|> spcSymb)


reservedWords :: ParsecT String u Identity String
reservedWords =  string "let"
             <|> string "par"
             <|> string "_"
             <|> string "!"
             <|> string "as"
             <|> string "forall"
             <|> string "exists"
             <|> string "NUMERAL"
             <|> string "DECIMAL"
             <|> string "STRING"

bValue :: ParsecT String u Identity Bool
bValue = (string "true" >> return True) <|>
         (string "false" >> return False)


-- Parse spec_constant
parseNumeral :: ParsecT String u Identity SpecConstant
parseNumeral = liftM SpecConstantNumeral (read  <$> numeral)

parseDecimal :: ParsecT String u Identity SpecConstant
parseDecimal = liftM SpecConstantDecimal decimal

parseHexadecimal :: ParsecT String u Identity SpecConstant
parseHexadecimal = liftM SpecContantHexadecimal hexadecimal

parseBinary :: ParsecT String u Identity SpecConstant
parseBinary = liftM SpecContantBinary (read <$> binary)

parseString :: ParsecT String u Identity SpecConstant
parseString = liftM SpecConstantString str



parseSpecConstant :: ParsecT String u Identity SpecConstant
parseSpecConstant = Pc.try parseDecimal
                <|> parseNumeral
                <|> parseHexadecimal
                <|> parseBinary
                <|> parseString



-- parse S-expressions, for the parsing to work newlines must be removed

parseSexprConstant :: ParsecT String u Identity Sexpr
parseSexprConstant = liftM SexprSpecConstant parseSpecConstant

parseSexprSymbol :: ParsecT String u Identity Sexpr
parseSexprSymbol = liftM SexprSymbol symbol

parseSexprKeyword :: ParsecT String u Identity Sexpr
parseSexprKeyword = liftM SexprSymbol keyword

parseSexprS :: ParsecT String u Identity Sexpr
parseSexprS = do
    aspO
    spaces
    res <- sepBy parseSexpr' spaces
    aspC
    spaces
    return $ SexprSxp res


parseSexpr' :: ParsecT String u Identity Sexpr
parseSexpr' =  parseSexprConstant
         <|> parseSexprSymbol
         <|> parseSexprKeyword
         <|> parseSexprS

parseSexpr :: ParsecT String u Identity [Sexpr]
parseSexpr = sepBy parseSexpr' spaces




-- Parse identifier
parseIdentifier :: ParsecT String u Identity Identifier
parseIdentifier = parseOnlySymbol <|> parseNSymbol

parseOnlySymbol :: ParsecT String u Identity Identifier
parseOnlySymbol = liftM ISymbol symbol

parseNSymbol :: ParsecT String u Identity Identifier
parseNSymbol =
    do aspO
       spaces
       aspUS
       spaces
       symb <- symbol
       spaces
       num <-  many1 (numeral <|> string " ")
       spaces
       aspC
       return $ I_Symbol symb num


-- Parse Sorts
parseSort :: ParsecT String u Identity [Sort]
parseSort = sepBy parseSort' spaces

parseSort' :: ParsecT String u Identity Sort
parseSort' = Pc.try parseIdentifierS <|> parseIdentifierSort

parseIdentifierS :: ParsecT String u Identity Sort
parseIdentifierS = liftM SortId parseIdentifier

parseIdentifierSort :: ParsecT String u Identity Sort
parseIdentifierSort = do
    aspO
    spaces
    identifier <- parseIdentifier
    spaces
    sorts <- sepBy1 parseSort' spaces
    spaces
    aspC
    return $ SortIdentifiers identifier sorts




--Parse Attribute Value
parseAttributeValue :: ParsecT String u Identity AttrValue
parseAttributeValue = parseAVSC <|> parseAVS <|> parseAVSexpr

parseAVSC :: ParsecT String u Identity AttrValue
parseAVSC = liftM AttrValueConstant parseSpecConstant

parseAVS :: ParsecT String u Identity AttrValue
parseAVS = liftM AttrValueSymbol symbol

parseAVSexpr :: ParsecT String u Identity AttrValue
parseAVSexpr = do
    aspO
    spaces
    expr <- parseSexpr
    aspC
    return $ AttrValueSexpr expr

-- Parse Attribute
parseAttribute :: ParsecT String u Identity Attribute
parseAttribute = do
    key <- keyword
    spaces
    attr <- optionMaybe parseAttributeValue
    case attr of
      Nothing -> return $ Attribute key
      Just val -> return $ AttributeVal key val




-- Parse terms

-- -- Parse Qual identifier
parseQualIdentifier :: ParsecT String u Identity QualIdentifier
parseQualIdentifier = Pc.try parseQID <|> parseQIAs

parseQID :: ParsecT String u Identity QualIdentifier
parseQID = liftM QIdentifier parseIdentifier

parseQIAs :: ParsecT String u Identity QualIdentifier
parseQIAs = do
  aspO
  spaces
  string "as"
  spaces
  ident <- parseIdentifier
  spaces
  sort <- parseSort'
  spaces
  aspC
  return $ QIdentifierAs ident sort


-- -- Parse Var Binding
parseVarBinding :: ParsecT String u Identity VarBinding
parseVarBinding = do
    aspO
    spaces
    symb <- symbol
    spaces
    term <- parseTerm
    spaces
    aspC
    return $ VB symb term




-- -- Parse Sorted Var
parseSortedVar :: ParsecT String u Identity SortedVar
parseSortedVar = do
    aspO
    spaces
    symb <- symbol
    spaces
    sort <- parseSort'
    spaces
    aspC
    return $ SV symb sort


-- Term
parseTerm :: ParsecT String u Identity Term
parseTerm =  parseTSPC
        <|> Pc.try parseTQID
        <|> Pc.try parseTQIT
        <|> Pc.try parseTermLet
        <|> Pc.try parseTermFA
        <|> Pc.try parseTermEX
        <|> parseTermAnnot

parseTSPC :: ParsecT String u Identity Term
parseTSPC = liftM TermSpecConstant parseSpecConstant

parseTQID :: ParsecT String u Identity Term
parseTQID = liftM TermQualIdentifier parseQualIdentifier

parseTQIT :: ParsecT String u Identity Term
parseTQIT = do
    aspO
    spaces
    iden <- parseQualIdentifier
    spaces
    terms <- sepBy1 parseTerm spaces
    aspC
    return $ TermQualIdentifierT iden terms

parseTermLet :: ParsecT String u Identity Term
parseTermLet = do
    aspO
    spaces
    string "let"
    spaces
    aspO
    spaces
    vb <- sepBy1 parseVarBinding spaces
    aspC
    spaces
    term <- parseTerm
    spaces
    aspC
    return $ TermLet vb term

parseTermFA :: ParsecT String u Identity Term
parseTermFA = do
    aspO
    spaces
    string "forall"
    spaces
    aspO
    sv <- sepBy1 parseSortedVar spaces
    aspC
    spaces
    term <- parseTerm
    spaces
    aspC
    return $ TermForall sv term


parseTermEX :: ParsecT String u Identity Term
parseTermEX = do
    aspO
    spaces
    string "exists"
    spaces
    aspO
    sv <- sepBy1 parseSortedVar spaces
    aspC
    spaces
    term <- parseTerm
    spaces
    aspC
    return $ TermExists sv term

parseTermAnnot :: ParsecT String u Identity Term
parseTermAnnot = do
  aspO
  spaces
  char '!'
  spaces
  term <- parseTerm
  spaces
  attr <- sepBy1 parseAttribute spaces
  aspC
  return $ TermAnnot term attr




--Parser for CmdGenResponse

parseCmdGenResponse :: ParsecT String u Identity CmdResponse
parseCmdGenResponse =
        (string "unsupported" >> return (CmdGenResponse Unsupported))
    <|> (string "success" >> return (CmdGenResponse Success))
    <|> parseCmdGenRepError

parseCmdGenRepError :: ParsecT String u Identity CmdResponse
parseCmdGenRepError = do
                  aspO
                  string "error"
                  space
                  char '"'
                  cont <- Pc.many (alphaNum<|> char ':'<|> char ' ')
                  char '"'
                  aspC
                  return $ CmdGenResponse $ CmdRsp.Error cont

-- Parsers for CmdGetInfoResponse

parseCmdGetInfoResponse :: ParsecT String u Identity CmdResponse
parseCmdGetInfoResponse = liftM CmdGetInfoResponse parseGetInfoResponse


parseGetInfoResponse :: ParsecT String u Identity [InfoResponse]
parseGetInfoResponse = do
    aspO
    spaces
    infoResp <- sepBy1 parseInfoResponse spaces
    aspC
    return infoResp


parseInfoResponse :: ParsecT String u Identity InfoResponse
parseInfoResponse =
    Pc.try parseResponseName <|>
    Pc.try parseResponseErrorBehavior <|>
    Pc.try parseResponseAuthors <|>
    Pc.try parseResponseVersion <|>
    Pc.try parseResponseReasonUnknown <|>
    parseResponseAttribute




parseResponseName :: ParsecT String u Identity InfoResponse
parseResponseName = string "name"  *> space *> liftM ResponseName str


parseResponseErrorBehavior :: ParsecT String u Identity InfoResponse
parseResponseErrorBehavior = string "error-behavior" *> space *>
                    liftM ResponseErrorBehavior parseErrorBehavior

parseErrorBehavior :: ParsecT String u Identity ErrorBehavior
parseErrorBehavior =
    (string "immediate-exit" >> return ImmediateExit) <|>
    (string "continued-execution" >> return ContinuedExecution)


parseResponseAuthors :: ParsecT String u Identity InfoResponse
parseResponseAuthors = string "authors" *> space *>
    liftM ResponseAuthors str

parseResponseVersion :: ParsecT String u Identity InfoResponse
parseResponseVersion = string "version" *> space *>
     liftM ResponseVersion str


parseResponseReasonUnknown :: ParsecT String u Identity InfoResponse
parseResponseReasonUnknown = string "reason" *> space *>
    liftM ResponseReasonUnknown parseReasonUnknown

parseResponseAttribute :: ParsecT String u Identity InfoResponse
parseResponseAttribute = liftM ResponseAttribute parseAttribute


-- Parser for check sat response
parseCheckSatResponse :: ParsecT String u Identity CheckSatResponse
parseCheckSatResponse =
        (string "sat" >> return Sat) <|>
        (string "unsat" >> return Unsat) <|>
        (string "unknown" >> return Unknown)

-- parse Get Assertion Response
parseGetAssertionResponse :: ParsecT String u Identity [Term]
parseGetAssertionResponse = do
    aspO
    spaces
    terms <- sepBy1 parseTerm spaces
    aspC
    return terms

-- parse Get Proof response
parseGetProofResponse :: ParsecT String u Identity Sexpr
parseGetProofResponse = parseSexpr'


-- parse Get unsat core response
parseGetUnsatCoreResp :: ParsecT String u Identity [String]
parseGetUnsatCoreResp = do
    aspO
    spaces
    symb <- sepBy1 symbol spaces
    aspC
    return symb


-- parse Get Value response
parseGetValueResponse :: ParsecT String u Identity [ValuationPair]
parseGetValueResponse = sepBy1 parseValuationPair spaces

parseValuationPair :: ParsecT String u Identity ValuationPair
parseValuationPair = do
    aspO
    spaces
    term1 <- parseTerm
    spaces
    term2 <- parseTerm
    spaces
    aspC
    return $ ValuationPair term1 term2


-- parse t valuation pair
parseTValuationPair :: ParsecT String u Identity TValuationPair
parseTValuationPair = do
    aspO
    spaces
    symb <- symbol
    spaces
    bval <- bValue
    spaces
    aspC
    return $ TValuationPair symb bval



-- parse get Assignent Response
parseGetAssignmentResp :: ParsecT String u Identity [TValuationPair]
parseGetAssignmentResp = do
    aspO
    spaces
    pairs <- sepBy1 parseTValuationPair spaces
    aspC
    return pairs


-- parse Get Option Response
parseGetOptionResponse :: ParsecT String u Identity AttrValue
parseGetOptionResponse = parseAttributeValue



parseReasonUnknown :: ParsecT String u Identity ReasonUnknown
parseReasonUnknown =
    (string "memout" >> return Memout) <|>
    (string "incomplete" >> return Incomplete)


--Parser for CmdResponse
{--
resultParser :: ParsecT String u Identity CmdResponse
resultParser = parseCmdGenResponse
            <|> parseCmdGetInfoResponse

--}

main :: IO a
main = forever $ do putStrLn "Enter a string: "
                    input <- getLine
                    parseTest parseCmdGenResponse input
                    --parseIdentifier
                    --parseTest parseTerm input
