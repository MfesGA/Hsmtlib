module Parser where

import Syntax
import AuxParser
import           Control.Applicative               as Ctr hiding ((<|>))
import           Control.Monad
import           Data.Functor.Identity
import           Hsmtlib.Solvers.Cmd.Parser.Syntax as CmdRsp
import           Text.Parsec.Prim                  as Prim
import           Text.ParserCombinators.Parsec     as Pc

-- parse Spec Constant

parseSpecConstant :: ParsecT String u Identity SpecConstant
parseSpecConstant = Pc.try parseDecimal
                <|> parseNumeral
                <|> parseHexadecimal
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
parseSexpr' = parseSexprConstant
          <|> parseSexprSymbol
          <|> 2parseSexprKeyword
          <|> parseSexprS

parseSexpr :: ParsecT String u Identity [Sexpr]
parseSexpr = sepBy parseSexpr' spaces