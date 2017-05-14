module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import Data.Maybe
import CoreParser
infixl 7 -#, #- 

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]  
iter m = m # iter m >-> cons ! return [] 

iterInt :: Parser a -> Int -> Parser [a]
iterInt m 0 = CoreParser.return []
iterInt m i = m # iterInt m (i - 1) >-> cons

cons(a, b) = a:b

(-#) :: Parser a -> Parser b -> Parser b
(m -# n) cs = (m # n >-> snd) cs

(#-) :: Parser a -> Parser b -> Parser a
(m #- n) cs = (m # n >-> fst) cs

space :: Parser Char
space m = (char ? isSpace) m

spaces :: Parser String
spaces =  iter space

token :: Parser a -> Parser a
token m = m #- spaces

letter :: Parser Char
letter string = (char ? isLetter) string

word :: Parser String
word = token (letter # iter letter >-> cons)

chars :: Int -> Parser String
chars n m = (iterInt CoreParser.char) n m

accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

require :: String -> Parser String
require m = (accept m) ! (err m)

--errorMsg :: Parser String -> String
--errorMsg m = "hej"
--	|m == Data.Maybe.isNothing = error "sister"
--	|otherwise = "ok sis"


lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char 
digit = char ? isDigit 

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')

