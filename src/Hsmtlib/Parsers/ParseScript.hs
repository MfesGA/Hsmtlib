module Hsmtlib.Parsers.ParseScript where

import           Control.Applicative           as Ctr hiding ((<|>))
import           Control.Monad
import           Data.Functor.Identity
import           Hsmtlib.Parsers.AuxParser
import           Hsmtlib.Parsers.Syntax
import           Text.Parsec.Prim              as Prim
import           Text.ParserCombinators.Parsec as Pc

teste  :: IO ()
teste =  getLine >>= readFile >>= pt.parse parseSource ""


{-
run :: Source -> IO ()
run source = do
  solver <- startSolver Z3 Online Nothing Nothing
  pint  $ toSMTLib2 solver source
-}

pt :: Either ParseError Source -> IO ()
pt (Left err) = print err
pt (Right x) = pt' x

pt' :: Source -> IO()
pt' = foldr (\ x -> (>>) (print x >> putStr "\n")) (return ())


{-
   ############################################´m#############################
   #                                                                       #
   #                       Parser for an SMTLib2 File                      #
   #                                                                       #
   #########################################################################
-}


parseSource :: ParsecT String u Identity Source
parseSource = Pc.many $ parseCommand <* Pc.try emptySpace


{-
  "###################### Parser For Commands ###############################
-}
parseCommand :: ParsecT String u Identity Command
parseCommand = Pc.try parseSetLogic
           <|> Pc.try parseSetOption
           <|> Pc.try parseSetInfo
           <|> Pc.try parseDeclareSort
           <|> Pc.try parseDefineSort
           <|> Pc.try parseDeclareFun
           <|> Pc.try parseDefineFun
           <|> Pc.try parsePush
           <|> Pc.try parsePop
           <|> Pc.try parseAssert
           <|> Pc.try parseCheckSat
           <|> Pc.try parseGetAssertions
           <|> Pc.try parseGetProof
           <|> Pc.try parseGetUnsatCore
           <|> Pc.try parseGetValue
           <|> Pc.try parseGetAssignment
           <|> Pc.try parseGetOption
           <|> Pc.try parseGetInfo
           <|> parseExit



{-
   #########################################################################
   #                                                                       #
   #                       Parser for each command                         #
   #                                                                       #
   #########################################################################
-}




parseSetLogic :: ParsecT String u Identity Command
parseSetLogic = do
  _ <- aspO
  _ <- emptySpace
  _ <- string "set-logic"
  _ <- emptySpace
  symb <- symbol
  _ <- emptySpace
  _ <- aspC
  return $ SetLogic symb


parseSetOption :: ParsecT String u Identity Command
parseSetOption = do
  _ <- aspO
  _ <- emptySpace
  _ <- string "set-option"
  _ <- emptySpace
  attr <- parseOption
  _ <- emptySpace
  _ <- aspC
  return $ SetOption attr

parseSetInfo :: ParsecT String u Identity Command
parseSetInfo = do
  _ <- aspO
  _ <- emptySpace
  _ <- string "set-info"
  _ <- emptySpace <?> "foi aqui1?"
  attr <- parseAttribute <?> "foi aqui2?"
  _ <- emptySpace
  _ <- aspC
  return $ SetInfo attr


parseDeclareSort :: ParsecT String u Identity Command
parseDeclareSort = do
  _ <- aspO
  _ <- emptySpace
  _ <- string "declare-sort"
  _ <- emptySpace
  symb <- symbol
  _ <- emptySpace
  nume <- numeral
  _ <- emptySpace
  _ <- aspC
  return $ DeclareSort symb (read nume :: Int)

parseDefineSort :: ParsecT String u Identity Command
parseDefineSort = do
  _ <- aspO
  _ <- emptySpace
  _ <- string "define-sort"
  _ <- emptySpace
  symb <- symbol
  _ <- emptySpace
  _ <- aspO
  symbs <- Pc.many $ symbol <* Pc.try emptySpace
  _ <- aspC
  _ <- emptySpace
  sort <- parseSort
  _ <- emptySpace
  _ <- aspC
  return $ DefineSort symb symbs sort


parseDeclareFun :: ParsecT String u Identity Command
parseDeclareFun = do
  _ <-aspO
  _ <- emptySpace
  _ <- string "declare-fun"
  _ <- emptySpace
  symb <- symbol
  _ <- emptySpace
  _ <- aspO
  _ <- emptySpace
  sorts <- Pc.many $ parseSort <* Pc.try emptySpace
  _ <- aspC
  _ <- emptySpace
  sort <- parseSort
  _ <- emptySpace
  _ <- aspC
  return $ DeclareFun symb sorts sort


parseDefineFun :: ParsecT String u Identity Command
parseDefineFun = do
  _ <- aspO
  _ <- emptySpace
  _ <- string "define-fun"
  _ <- emptySpace
  symb <- symbol
  _ <- emptySpace
  _ <- aspO
  sVars <- Pc.many $ parseSortedVar <* Pc.try emptySpace
  _ <- aspC
  _ <- emptySpace
  sort <- parseSort
  _ <- emptySpace
  term <- parseTerm
  _ <- emptySpace
  _ <- aspC
  return $ DefineFun symb sVars sort term


parsePush :: ParsecT String u Identity Command
parsePush = do
  _ <- aspO
  _ <- emptySpace
  _ <- string "push"
  _ <- emptySpace
  nume <- numeral
  _ <- emptySpace
  _ <- aspC
  return $ Push (read nume :: Int)


parsePop :: ParsecT String u Identity Command
parsePop = do
  _ <- aspO
  _ <- emptySpace
  _ <- string "pop"
  _ <- emptySpace
  nume <- numeral
  _ <- emptySpace
  _ <- aspC
  return $ Pop (read nume :: Int)



parseAssert :: ParsecT String u Identity Command
parseAssert = do
  _ <- aspO
  _ <- emptySpace
  _ <- string "assert"
  _ <- emptySpace
  term <- parseTerm
  _ <- emptySpace
  _ <- aspC
  return $ Assert term


parseCheckSat :: ParsecT String u Identity Command
parseCheckSat = do
  _ <- aspO
  _ <- emptySpace
  _ <- string "check-sat"
  _ <- emptySpace
  _ <- aspC
  return CheckSat


parseGetAssertions :: ParsecT String u Identity Command
parseGetAssertions = do
  _ <- aspO
  _ <-emptySpace
  _ <- string "get-assertions"
  _ <- emptySpace
  _ <- aspC
  return GetAssertions

parseGetProof :: ParsecT String u Identity Command
parseGetProof = do
  _ <- aspO
  _ <- emptySpace
  _ <- string "get-proof"
  _ <- emptySpace
  _ <-aspC
  return GetProof

parseGetUnsatCore :: ParsecT String u Identity Command
parseGetUnsatCore = do
  _ <- aspO
  _ <- emptySpace
  _ <- string "get-unsat-core"
  _ <- emptySpace
  _ <- aspC
  return GetUnsatCore

parseGetValue :: ParsecT String u Identity Command
parseGetValue = do
  _ <- aspO
  _ <- emptySpace
  _ <- string "get-value"
  _ <- emptySpace
  _ <- aspO
  terms <- Pc.many1 $ parseTerm <* Pc.try emptySpace
  _ <- aspC
  _ <- emptySpace
  _ <- aspC
  return $ GetValue terms

parseGetAssignment :: ParsecT String u Identity Command
parseGetAssignment = do
  _ <- aspO
  _ <- emptySpace
  _ <- string "get-assignment"
  _ <- emptySpace
  _ <- aspC
  return GetAssignment


parseGetOption :: ParsecT String u Identity Command
parseGetOption = do
  _ <- aspO
  _ <- emptySpace
  _ <- string "get-option"
  _ <- emptySpace
  word <- keyword
  _ <- emptySpace
  _ <- aspC
  return $ GetOption word


parseGetInfo :: ParsecT String u Identity Command
parseGetInfo = do
  _ <- aspO
  _ <- emptySpace
  _ <- string "get-info"
  _ <- emptySpace
  flag <- parseInfoFlags
  _ <- emptySpace
  _ <- aspC
  return $ GetInfo flag



parseExit :: ParsecT String u Identity Command
parseExit = do
  _ <- aspO
  _ <- emptySpace
  _ <- string "exit"
  _ <- emptySpace
  _ <- aspC
  return Exit




{-
   #########################################################################
   #                                                                       #
   #                       Parse Command Options                           #
   #                                                                       #
   #########################################################################
-}


-- parseBool

parseBool :: ParsecT String u Identity Bool
parseBool = (true *> return True) <|> (false *> return False)


parseOption :: ParsecT String u Identity Option
parseOption = parsePrintSuccess
          <|> parseExpandDefinitions
          <|> parseInteractiveMode
          <|> parseProduceProofs
          <|> parseProduceUnsatCores
          <|> parseProduceModels
          <|> parseProduceAssignments
          <|> parseRegularOutputChannel
          <|> parseDiagnosticOutputChannel
          <|> parseRandomSeed
          <|> parseVerbosity
          <|> parseOptionAttribute


-- parse PrintSucess
parsePrintSuccess :: ParsecT String u Identity Option
parsePrintSuccess = do
  _ <- string ":print-success"
  _ <- spaces
  val <- parseBool
  return $ PrintSucess val


parseExpandDefinitions :: ParsecT String u Identity Option
parseExpandDefinitions = do
  _ <- string ":expand-definitions"
  _ <- spaces
  val <- parseBool
  return $ ExpandDefinitions val


parseInteractiveMode :: ParsecT String u Identity Option
parseInteractiveMode = do
  _ <- string ":interactive-mode"
  _ <- spaces
  val <- parseBool
  return $ InteractiveMode val

parseProduceProofs :: ParsecT String u Identity Option
parseProduceProofs = do
  _ <- string ":produce-proofs"
  _ <- spaces
  val <- parseBool
  return $ ProduceProofs val


parseProduceUnsatCores :: ParsecT String u Identity Option
parseProduceUnsatCores = do
  _ <- string ":produce-unsat-cores"
  _ <- spaces
  val <- parseBool
  return $ ProduceUnsatCores val


parseProduceModels :: ParsecT String u Identity Option
parseProduceModels = do
  _ <- string ":produce-models"
  _ <- spaces
  val <- parseBool
  return $ ProduceModels val

parseProduceAssignments :: ParsecT String u Identity Option
parseProduceAssignments = do
  _ <- string ":produce-assignnments"
  _ <- spaces
  val <- parseBool
  return $  ProduceAssignments val


parseRegularOutputChannel :: ParsecT String u Identity Option
parseRegularOutputChannel = do
  _ <- string ":regular-output-channel"
  _ <- spaces
  val <- str
  return $ RegularOutputChannel val


parseDiagnosticOutputChannel :: ParsecT String u Identity Option
parseDiagnosticOutputChannel = do
  _ <- string ":diagnostic-output-channel"
  _ <- spaces
  val <- str
  return $ DiagnosticOutputChannel val

parseRandomSeed :: ParsecT String u Identity Option
parseRandomSeed = do
  _ <- string ":random-seed"
  _ <- spaces
  val <- numeral
  return $ RandomSeed (read val :: Int)


parseVerbosity :: ParsecT String u Identity Option
parseVerbosity = do
  _ <- string ":verbosity"
  _ <- spaces
  val <- numeral
  return $ Verbosity (read val :: Int)


parseOptionAttribute :: ParsecT String u Identity Option
parseOptionAttribute = do
  attr <- parseAttribute
  return $ OptionAttr attr




{-
   #########################################################################
   #                                                                       #
   #                       Parsers for Info FLags                          #
   #                                                                       #
   #########################################################################
-}

parseInfoFlags :: ParsecT String u Identity InfoFlags
parseInfoFlags = parseErrorBehaviour
             <|> parseName
             <|> parseAuthors
             <|> parseVersion
             <|> parseStatus
             <|> parseReasonUnknown
             <|> parseInfoKeyword
             <|> parseAllStatistics


parseErrorBehaviour :: ParsecT String u Identity InfoFlags
parseErrorBehaviour = string ":error-behaviour" *> return ErrorBehavior


parseName :: ParsecT String u Identity InfoFlags
parseName = string ":name" *> return Name

parseAuthors :: ParsecT String u Identity InfoFlags
parseAuthors = string ":authors" *> return Authors

parseVersion :: ParsecT String u Identity InfoFlags
parseVersion = string ":version" *> return Version

parseStatus :: ParsecT String u Identity InfoFlags
parseStatus = string ":status" *> return Status

parseReasonUnknown :: ParsecT String u Identity InfoFlags
parseReasonUnknown = string ":reason-unknown" *> return  ReasonUnknown

parseAllStatistics :: ParsecT String u Identity InfoFlags
parseAllStatistics = string ":all-statistics" *> return AllStatistics

parseInfoKeyword :: ParsecT String u Identity InfoFlags
parseInfoKeyword = liftM InfoFlags keyword




{-
   #########################################################################
   #                                                                       #
   #                          Parsers for Terms                            #
   #                                                                       #
   #########################################################################
-}


-- Term
parseTerm :: ParsecT String u Identity Term
parseTerm = parseTSPC
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
    _ <- aspO
    _ <- emptySpace
    iden <- parseQualIdentifier
    _ <- emptySpace
    terms <- Pc.many $ parseTerm <* Pc.try emptySpace
    _ <- aspC
    return $ TermQualIdentifierT iden terms

parseTermLet :: ParsecT String u Identity Term
parseTermLet = do
    _ <- aspO
    _ <- emptySpace
    _ <- string "let"
    _ <- emptySpace
    _ <- aspO
    _ <- emptySpace
    vb <- Pc.many $ parseVarBinding <* Pc.try emptySpace
    _ <- aspC
    _ <- emptySpace
    term <- parseTerm
    _ <- emptySpace
    _ <- aspC
    return $ TermLet vb term

parseTermFA :: ParsecT String u Identity Term
parseTermFA = do
    _ <- aspO
    _ <- emptySpace
    _ <- string "forall"
    _ <- emptySpace
    _ <- aspO
    sv <- Pc.many $ parseSortedVar <* Pc.try emptySpace
    _ <- aspC
    _ <- emptySpace
    term <- parseTerm
    _ <- emptySpace
    _ <- aspC
    return $ TermForall sv term


parseTermEX :: ParsecT String u Identity Term
parseTermEX = do
    _ <- aspO
    _ <- emptySpace
    _ <- string "exists"
    _ <- emptySpace
    _ <- aspO
    sv <- Pc.many $ parseSortedVar <* Pc.try emptySpace
    _ <- aspC
    _ <- emptySpace
    term <- parseTerm
    _ <- emptySpace
    _ <- aspC
    return $ TermExists sv term

parseTermAnnot :: ParsecT String u Identity Term
parseTermAnnot = do
  _ <- aspO
  _ <- emptySpace
  _ <- char '!'
  _ <- emptySpace
  term <- parseTerm
  _ <- emptySpace
  attr <- Pc.many $ parseAttribute <* Pc.try emptySpace
  _ <- aspC
  return $ TermAnnot term attr


-- -- Parse Sorted Var
parseSortedVar :: ParsecT String u Identity SortedVar
parseSortedVar = do
    _ <- aspO
    _ <- emptySpace
    symb <- symbol
    _ <- emptySpace
    sort <- parseSort
    _ <- emptySpace
    _ <- aspC
    return $ SV symb sort

-- -- Parse Qual identifier
parseQualIdentifier :: ParsecT String u Identity QualIdentifier
parseQualIdentifier = Pc.try parseQID <|> parseQIAs

parseQID :: ParsecT String u Identity QualIdentifier
parseQID = liftM QIdentifier parseIdentifier




parseQIAs :: ParsecT String u Identity QualIdentifier
parseQIAs = do
  _ <- aspO
  _ <- emptySpace
  _ <- string "as"
  _ <- emptySpace
  ident <- parseIdentifier
  _ <- emptySpace
  sort <- parseSort
  _ <- emptySpace
  _ <- aspC
  return $ QIdentifierAs ident sort


-- -- Parse Var Binding
parseVarBinding :: ParsecT String u Identity VarBinding
parseVarBinding = do
    _ <- aspO
    _ <- emptySpace
    symb <- symbol
    _ <- emptySpace
    term <- parseTerm
    _ <- emptySpace
    _ <- aspC
    return $ VB symb term





{-
   #########################################################################
   #                                                                       #
   #                          Parsers for Attributes                       #
   #                                                                       #
   #########################################################################
-}

--Parse Attribute Value
parseAttributeValue :: ParsecT String u Identity AttrValue
parseAttributeValue = parseAVSC <|> parseAVS <|> parseAVSexpr

parseAVSC :: ParsecT String u Identity AttrValue
parseAVSC = liftM AttrValueConstant parseSpecConstant

parseAVS :: ParsecT String u Identity AttrValue
parseAVS = liftM AttrValueSymbol symbol

parseAVSexpr :: ParsecT String u Identity AttrValue
parseAVSexpr = do
    _ <- aspO
    _ <- emptySpace
    expr <- Pc.many $ parseSexpr <* Pc.try emptySpace
    _ <- aspC
    return $ AttrValueSexpr expr



-- Parse Attribute

parseAttribute :: ParsecT String u Identity Attribute
parseAttribute =  Pc.try parseKeyAttAttribute <|> parseKeyAttribute


parseKeyAttribute :: ParsecT String u Identity Attribute
parseKeyAttribute = liftM  Attribute keyword

parseKeyAttAttribute :: ParsecT String u Identity Attribute
parseKeyAttAttribute = do
  kw <- keyword
  _ <- emptySpace
  atr <- parseAttributeValue
  return $ AttributeVal kw atr


{-
   #########################################################################
   #                                                                       #
   #                          Parsers Sort                                 #
   #                                                                       #
   #########################################################################
-}

-- Parse Sot

parseSort :: ParsecT String u Identity Sort
parseSort = Pc.try parseIdentifierS <|> parseIdentifierSort

parseIdentifierS :: ParsecT String u Identity Sort
parseIdentifierS = liftM SortId parseIdentifier

parseIdentifierSort :: ParsecT String u Identity Sort
parseIdentifierSort = do
    _ <- aspO
    _ <- emptySpace
    identifier <- parseIdentifier
    _ <- emptySpace
    sorts <- many1 (parseSort  <* Pc.try emptySpace)
    _ <- aspC
    return $ SortIdentifiers identifier sorts




{-
   #########################################################################
   #                                                                       #
   #                          Parsers Identifiers                          #
   #                                                                       #
   #########################################################################
-}


-- Parse Identifiers

parseIdentifier :: ParsecT String u Identity Identifier
parseIdentifier = parseOnlySymbol <|> parseNSymbol

parseOnlySymbol :: ParsecT String u Identity Identifier
parseOnlySymbol = liftM ISymbol symbol

parseNSymbol :: ParsecT String u Identity Identifier
parseNSymbol = do
       _ <- aspO
       _ <- emptySpace
       _ <- aspUS
       _ <- emptySpace
       symb <- symbol
       _ <- emptySpace
       nume <- many1  (numeral <* Pc.try spaces)
       _ <- aspC
       return $ I_Symbol symb (fmap (read) nume)

{-
   #########################################################################
   #                                                                       #
   #                          Parsers S-exprs                              #
   #                                                                       #
   #########################################################################
-}


-- parse S-expressions

parseSexprConstant :: ParsecT String u Identity Sexpr
parseSexprConstant = liftM SexprSpecConstant parseSpecConstant

parseSexprSymbol :: ParsecT String u Identity Sexpr
parseSexprSymbol = liftM SexprSymbol symbol

parseSexprKeyword :: ParsecT String u Identity Sexpr
parseSexprKeyword = liftM SexprSymbol keyword

parseAtomSexpr :: ParsecT String u Identity Sexpr
parseAtomSexpr = parseSexprConstant
          <|> parseSexprSymbol
          <|> parseSexprKeyword


parseListSexpr :: ParsecT String u Identity Sexpr
parseListSexpr = do
    _ <- aspO
    list <- Pc.many parseSexpr
    _ <- aspC
    return $ SexprSxp  list



parseSexpr :: ParsecT String u Identity Sexpr
parseSexpr = do
  _ <- emptySpace
  expr <- parseAtomSexpr <|> parseListSexpr
  _ <- emptySpace
  return expr



-- parse Spec Constant
parseSpecConstant :: ParsecT String u Identity SpecConstant
parseSpecConstant = Pc.try parseDecimal
                <|> parseNumeral
                <|> Pc.try parseHexadecimal
                <|> parseBinary
                <|> parseString



parseNumeral :: ParsecT String u Identity SpecConstant
parseNumeral = liftM SpecConstantNumeral (read  <$> numeral)

parseDecimal :: ParsecT String u Identity SpecConstant
parseDecimal = liftM SpecConstantDecimal decimal

parseHexadecimal :: ParsecT String u Identity SpecConstant
parseHexadecimal = liftM SpecConstantHexadecimal hexadecimal

parseBinary :: ParsecT String u Identity SpecConstant
parseBinary = liftM SpecConstantBinary binary

parseString :: ParsecT String u Identity SpecConstant
parseString = liftM SpecConstantString str

































