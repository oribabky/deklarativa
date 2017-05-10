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

toUpper1 :: Parser Char
toUpper1 = letter1 >-> toUpper

sndChar1 :: Parser Char
sndChar1 = twoChars1 >-> snd

twoChars2 :: Parser String
twoChars2 = twoChars1 >-> tupleToString
	where
	tupleToString :: (Char, Char) -> String
	tupleToString (a, b) = [a] ++ [b]

--Parser operators
(-#) :: Parser a -> Parser b -> Parser b
(m -# n) cs = (m # n >-> snd) cs

(#-) :: Parser a -> Parser b -> Parser a
(m #- n) cs = (m # n >-> fst) cs


iterate1 :: Parser a -> Int -> Parser [a]
iterate1 m 0 = CoreParser.return []
iterate1 m i = m # iterate1 m (i - 1) >-> cons1

--constructs a list from a parser tuple.
cons1 :: (a, [a]) -> [a]
cons1 (hd, tl) = hd:tl

iterate2 :: Parser a -> Parser [a]
iterate2 m = (((m # (iterate2 m))) >-> cons1) ! (CoreParser.return [])

letters1 :: Parser String
letters1 = iterate2 letter1

--token :: Parser a -> Parser a
--token m = m #