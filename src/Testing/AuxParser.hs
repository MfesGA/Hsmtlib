module AuxParser where

{-|
    In the String terminal, it does not parse C-style characters.
    Quoted Symbol does not parse all printable ASCII characters.
-}

import           Data.Functor.Identity                    (Identity)
import           Text.Parsec.Prim                         as Prim (ParsecT,
                                                                   unexpected)
import           Text.ParserCombinators.Parsec.Char       (char, digit, letter,
                                                           string)

import           Control.Applicative
import           Text.ParserCombinators.Parsec.Combinator (many1)


(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) a b = (++) <$> a <*> b


-- Parse a Numeral

numeral :: ParsecT String u Identity String
numeral = many1 digit


-- Parse a decimal

decimal :: ParsecT String u Identity String
decimal = numeral <++>  dot <++> zeros <++> numeral

zeros :: ParsecT String u Identity String
zeros = Pc.many $ char '0'


dot :: ParsecT String u Identity String
dot = string "."

-- parse a Hexadecimal

hexadecimal :: ParsecT String u Identity String
hexadecimal = string "#x" *> many1 hexDigit


--parsea a Binary
binary :: ParsecT String u Identity String
binary = string "#b" *> many1 bin

bin :: ParsecT String u Identity Char
bin = char '0' <|> char '1'


--parse a String
-- Dosent parse strings with escape characters
str :: ParsecT String u Identity String
str = string "\"" <++> Pc.many strChar <++> string "\""

strChar :: ParsecT String u Identity Char
strChar = alphaNum <|> char ' '

--parse a Symbol
symbol :: ParsecT String u Identity String
symbol = simpleSymbol <|> quotedSymbol

quotedSymbol :: ParsecT String u Identity String
quotedSymbol = char '|' <:> Pc.many alphaNum <++> string "|"

simpleSymbol :: ParsecT String u Identity String
simpleSymbol = (letter <|> spcSymb) <:>  sq
    where sq = Pc.many (alphaNum <|> spcSymb)

spcSymb :: ParsecT String u Identity Char
spcSymb = oneOf  "+-/*=%?!.$_~^&<>"

-- parse a key word
keyword :: ParsecT String u Identity String
keyword = char ':' <:> Pc.many (alphaNum<|> spcSymb)


aspO :: ParsecT String u Identity Char
aspO = char '('

aspC :: ParsecT String u Identity Char
aspC = char ')'

aspUS :: ParsecT String u Identity Char
aspUS = char '_'

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