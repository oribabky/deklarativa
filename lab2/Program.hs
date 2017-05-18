module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Data.Maybe
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] -- to be defined

instance Parse T where
  parse = iter Statement.parse >-> Program
  toString = convertToString
  	where
  		convertToString :: T -> String
  		convertToString (Program []) = ""
  		convertToString (Program (stmt : stmts)) = Statement.toString stmt ++ convertToString (Program stmts) 
             
exec :: T -> [Integer] -> [Integer]
exec (Program p) allowedInts = Statement.exec p dict allowedInts
	where
		dict = Dictionary.empty




