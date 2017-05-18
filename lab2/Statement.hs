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
    Begin [Statement] |
    Repeat Statement Expr.T
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

repeat1 = accept "repeat" -# statement # require "until" -# Expr.parse #- require ";" >-> buildRepeat
buildRepeat (a, b) = Repeat a b

statement = assignment ! if1 ! while ! read1 ! write ! skip ! begin ! repeat1

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] dict input = []

exec (Assignment varName value : stmts) dict input = exec stmts newDict input
	where
		newDict = Dictionary.insert (varName, Expr.value value dict) dict

exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec (While cond stmt : stmts) dict input = 
    if (Expr.value cond dict)>0 
	then exec (stmt : While cond stmt : stmts) dict input
    else exec stmts dict input    

exec (Read varName : stmts) dict (usedNr : numbers) = exec stmts newDict numbers
	where
		newDict = Dictionary.insert (varName, usedNr) dict

exec (Write value : stmts) dict input = (val : exec stmts dict input)
	where
		val = Expr.value value dict

exec (Skip : stmts) dict input = exec stmts dict input

exec (Begin beginStmts : stmts) dict input = exec (beginStmts ++ stmts) dict input

exec (Repeat stmt cond : stmts) dict input = 
    exec (stmt : ((If cond Skip (Repeat stmt cond)): stmts)) dict input


stringStmt :: T -> String

stringStmt (Assignment varName value) = varName ++ " := " ++ toString value ++ ";\n"

stringStmt (If cond thenStmts elseStmts) = "if " ++ toString cond ++ " then\n" 
	++ stringStmt thenStmts ++ "else\n" ++ stringStmt elseStmts

stringStmt (While cond stmt) = "while " ++ toString cond ++ " do\n" ++ stringStmt stmt

stringStmt (Read varName) = "read " ++ varName ++ ";\n"

stringStmt (Write value) = "write " ++ toString value ++ ";\n"

stringStmt (Skip) = "skip;\n"

stringStmt (Begin stmts) = "begin\n" ++ stringStmtList stmts ++ "end\n"

stringStmt (Repeat stmt cond) = "repeat\n" ++ stringStmt stmt ++ "until " ++ toString cond ++ ";\n"

stringStmtList :: [T] -> String
stringStmtList [] = ""
stringStmtList (x : xs) = stringStmt x ++ stringStmtList xs


instance Parse Statement where
  parse = statement

  toString = stringStmt
