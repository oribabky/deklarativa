module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
import Data.Maybe
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Read String |
    Write Expr.T |
    Skip |
    Begin [Statement]
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

if1 = accept "if" -# Expr.parse #- require "then" #
	statement #- accept "else" # statement >-> buildIf
buildIf ((a, b), c) = If a b c

while = accept "while" -# Expr.parse # require "do" -# statement >-> buildWhile
buildWhile (a, b) = While a b

read1 = accept "read" -# word #- require ";" >-> buildRead
buildRead a = Read a

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite a = Write a

skip = accept "skip;" >-> buildSkip
buildSkip _ = Skip

begin = accept "begin" -# iter statement #- require "end" >-> buildBegin
buildBegin a = Begin a

statement = assignment ! if1 ! while ! read1 ! write ! skip ! begin

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (Assignment varName value : stmts) dict input = exec stmts newDict input
	where
		newDict = Dictionary.insert (varName, Expr.value value dict) dict

exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec (While cond stmt : stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (stmt : stmts) dict input
    else exec stmts dict input    

exec (Read varName : stmts) dict input = exec stmts dict newInput
	where
		newInput = newInput ++ [fromJust(Dictionary.lookup varName dict)]

exec (Write value : stmts) dict input = exec stmts dict input ++ [val]
	where
		val = Expr.value value dict

exec (Skip : stmts) dict input = exec stmts dict input

exec (Begin beginStmts : stmts) dict input = exec (beginStmts ++ stmts) dict input

stringStmt :: T -> String
stringStmt (Assignment varName value) = v ++ " := " ++ toString e ++ ";\n"

stringStmt (If cond thenStmts elseStmts) = "if " ++ toString cond ++ " then\n" 
	++ stringStmt thenStmts ++ "else\n" ++ stringStmt elseStmts

stringStmt (While cond stmt) = "while " ++ toString cond ++ " do\n" ++ stringStmt stmt

stringStmt (Read varName) = "read " ++ varName ++ ";\n"

stringStmt (Write value) = "write " ++ toString value ++ ";\n"

stringStmt (Skip) = "skip;\n"
instance Parse Statement where
  parse = statement

  toString = stringStmt
