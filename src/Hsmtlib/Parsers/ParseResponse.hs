module Hsmtlib.Parsers.ParseResponse where

import           Control.Applicative           as Ctr hiding ((<|>))
import           Control.Monad
import           Data.Functor.Identity
import           Hsmtlib.Parsers.AuxParser
import           Hsmtlib.Parsers.ParseScript
import           Hsmtlib.Parsers.Syntax        as CmdRsp
import           Text.Parsec.Prim              as Prim
import           Text.ParserCombinators.Parsec as Pc



parseCmdResult :: ParsecT String u Identity CmdResponse
parseCmdResult = Pc.try parseCmdGenResponse
             <|> Pc.try parseCmdCheckSatResponse
             <|> Pc.try parseCmdGetInfoResponse
             <|> Pc.try parseCmdGetAssertion 
             <|> Pc.try parseCmdGetAssignment
             <|> Pc.try parseCmdGetProof
             <|> Pc.try parseCmdGetProof
             <|> Pc.try parseCmdGetValueResponse
             <|> parseCmdGetOptionResponse

{-
   #########################################################################
   #                                                                       #
   #                       Parser Cmd Gen Response                         #
   #                                                                       #
   #########################################################################
-}

parseCmdGenResponse :: ParsecT String u Identity CmdResponse
parseCmdGenResponse = liftM CmdGenResponse parseGenResponse

parseGenResponse :: ParsecT String u Identity GenResponse
parseGenResponse =  parseUnsupported <|> parseSuccess <|> parseGenError

parseSuccess :: ParsecT String u Identity GenResponse
parseSuccess = string "success" *> return Success

parseUnsupported :: ParsecT String u Identity GenResponse
parseUnsupported = string "unsupported" *> return Unsupported


parseGenError :: ParsecT String u Identity GenResponse
parseGenError = do
    _ <- aspO
    _ <- emptySpace
    _ <- string "error"
    _ <- emptySpace
    err <- str
    _ <- emptySpace
    _ <- aspC
    return $ CmdRsp.Error err

  
   



{-
   #########################################################################
   #                                                                       #
   #                       Parser get info response                        #
   #                                                                       #
   #########################################################################
-}

parseCmdGetInfoResponse :: ParsecT String u Identity CmdResponse
parseCmdGetInfoResponse = liftM CmdGetInfoResponse parseGetInfoResponse


parseGetInfoResponse :: ParsecT String u Identity [InfoResponse]
parseGetInfoResponse = do
    _ <-aspO
    _ <- emptySpace
    infoResp <- Pc.many $ parseInfoResponse <* Pc.try emptySpace
    _ <- aspC
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
parseResponseName = string ":name"  *> emptySpace *> liftM ResponseName str


parseResponseErrorBehavior :: ParsecT String u Identity InfoResponse
parseResponseErrorBehavior = string ":error-behavior" *> emptySpace *>
                    liftM ResponseErrorBehavior parseErrorBehavior

parseErrorBehavior :: ParsecT String u Identity ErrorBehavior
parseErrorBehavior =
    (string "immediate-exit" >> return ImmediateExit) <|>
    (string "continued-execution" >> return ContinuedExecution)


parseResponseAuthors :: ParsecT String u Identity InfoResponse
parseResponseAuthors = string ":authors" *> emptySpace *>
    liftM ResponseAuthors str

parseResponseVersion :: ParsecT String u Identity InfoResponse
parseResponseVersion = string ":version" *> emptySpace *>
     liftM ResponseVersion str



parseResponseReasonUnknown :: ParsecT String u Identity InfoResponse
parseResponseReasonUnknown = string "reason" *> emptySpace *>
    liftM ResponseReasonUnknown parseRReasonUnknown

parseRReasonUnknown :: ParsecT String u Identity ReasonUnknown
parseRReasonUnknown =
    (string "memout" >> return Memout) <|>
    (string "incomplete" >> return Incomplete)



parseResponseAttribute :: ParsecT String u Identity InfoResponse
parseResponseAttribute = liftM ResponseAttribute parseAttribute 







{-
   #########################################################################
   #                                                                       #
   #                       Parser check sat response                       #
   #                                                                       #
   #########################################################################
-}



parseCmdCheckSatResponse :: ParsecT String u Identity CmdResponse
parseCmdCheckSatResponse = liftM  CmdCheckSatResponse parseCheckSatResponse



-- Parser for check sat response
parseCheckSatResponse :: ParsecT String u Identity CheckSatResponse
parseCheckSatResponse =
    (string "sat" >> return Sat) <|>
    (string "unsat" >> return Unsat) <|>
    (string "unknown" >> return Unknown)




{-
   #########################################################################
   #                                                                       #
   #                       Parser get assertions cmd                       #
   #                                                                       #
   #########################################################################
-}


parseCmdGetAssertion :: ParsecT String u Identity CmdResponse
parseCmdGetAssertion = liftM CmdGetAssertionResponse parseGetAssertionResponse



-- parse Get Assertion Response
parseGetAssertionResponse :: ParsecT String u Identity [Term]
parseGetAssertionResponse = do
    _ <- aspO
    _ <- emptySpace
    terms <- Pc.many $ parseTerm <* Pc.try emptySpace
    _ <- aspC
    return terms

{-
   #########################################################################
   #                                                                       #
   #                       Parser get proof response                       #
   #                                                                       #
   #########################################################################
-}


parseCmdGetProof :: ParsecT String u Identity CmdResponse
parseCmdGetProof = liftM CmdGetProofResponse parseGetProofResponse

-- parse Get Proof response
parseGetProofResponse :: ParsecT String u Identity Sexpr
parseGetProofResponse = parseSexpr



{-
   #########################################################################
   #                                                                       #
   #                       Parser get unsat core response                  #
   #                                                                       #
   #########################################################################
-}


parseCmdGetUnsatCore :: ParsecT String u Identity CmdResponse
parseCmdGetUnsatCore =  liftM CmdGetUnsatCoreResoponse parseGetUnsatCoreResp


-- parse Get unsat core response
parseGetUnsatCoreResp :: ParsecT String u Identity [String]
parseGetUnsatCoreResp = do
    _ <- aspO
    _ <- emptySpace
    symb <- Pc.many $ symbol <* Pc.try emptySpace
    _ <- aspC
    return symb


{-
   #########################################################################
   #                                                                       #
   #                       Parser Cmd Get value response                   #
   #                                                                       #
   #########################################################################
-}


parseCmdGetValueResponse :: ParsecT String u Identity CmdResponse
parseCmdGetValueResponse = liftM CmdGetValueResponse parseGetValueResponse



-- parse Get Value response
parseGetValueResponse :: ParsecT String u Identity [ValuationPair]
parseGetValueResponse =
    aspO *> (Pc.many $ parseValuationPair <* Pc.try emptySpace) <* aspC

parseValuationPair :: ParsecT String u Identity ValuationPair
parseValuationPair = do
    _ <- aspO
    _ <- emptySpace
    term1 <- parseTerm
    _ <- emptySpace
    term2 <- parseTerm
    _ <- emptySpace
    _ <- aspC
    return $ ValuationPair term1 term2


{-
   #########################################################################
   #                                                                       #
   #                       Parser Cmd get assignment Resp                  #
   #                                                                       #
   #########################################################################
-}


parseCmdGetAssignment :: ParsecT String u Identity CmdResponse
parseCmdGetAssignment = liftM CmdGetAssignmentResponse parseGetAssignmentResp

-- parse get Assignent Response
parseGetAssignmentResp :: ParsecT String u Identity [TValuationPair]
parseGetAssignmentResp = do
    _ <- aspO
    _ <- emptySpace
    pairs <- Pc.many $ parseTValuationPair <* Pc.try emptySpace
    _ <- aspC
    return pairs

-- parse t valuation pair
parseTValuationPair :: ParsecT String u Identity TValuationPair
parseTValuationPair = do
    _ <- aspO
    _ <- emptySpace
    symb <- symbol
    _ <- emptySpace
    bval <- parseBool
    _ <- emptySpace
    _ <-aspC
    return $ TValuationPair symb bval





{-
   #########################################################################
   #                                                                       #
   #                       Parser Cmd get option response                  #
   #                                                                       #
   #########################################################################
-}


parseCmdGetOptionResponse :: ParsecT String u Identity CmdResponse
parseCmdGetOptionResponse = liftM CmdGetOptionResponse parseGetOptionResponse


-- parse Get Option Response
parseGetOptionResponse :: ParsecT String u Identity AttrValue
parseGetOptionResponse = parseAttributeValue



