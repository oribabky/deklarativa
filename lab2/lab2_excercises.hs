import Data.Char
import CoreParser

--type Parser a = String -> Maybe(a, String)

semiColon :: Parser Char
semiColon (';' : xs) = Just(';', xs)
semiColon _ = Nothing


char1 :: Parser Char
char1 (x : xs) = Just(x, xs)
char1 [] = Nothing

digit1 :: Parser Char
digit1 num = (char ? isDigit) num

letter1 :: Parser Char
letter1 string = (char ? isLetter) string

alphaNum1 :: Parser Char
alphaNum1 string = (letter1 ! digit1) string

lit1 :: Char -> Parser Char
lit1 c string = (char1 ? (==c)) string

semiColon2 :: Parser Char
semiColon2 = lit1 ';'

twoChars1 :: Parser (Char, Char)
twoChars1 = char1 # char1