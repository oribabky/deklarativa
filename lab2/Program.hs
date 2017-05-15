module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Data.Maybe
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] -- to be defined

instance Parse T where
  parse = iter Statement.parse >-> Program
  toString = error "Program.toString not implemented"
             
exec :: T -> [Integer] -> [Integer]
exec (Program p) allowedInts = Statement.exec p dict allowedInts
	where
		dict = Dictionary.empty


