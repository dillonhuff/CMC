module LexerTests() where

import ErrorHandling
import Lexer
import TestUtils

lexerTests = testFunction (extractValue . lexer) lexCases

lexCases =
	[plusOP
	,minusOP
	,timesOP
	,scalarTimesOP
	,invertOp
	,transposeOp
	,varName
	,longVarName
	,digitsOnlyNum
	,digitsAndExpNum
	,digitsAndUpperExpNum
	,decimalNum
	,decimalAndExp
	,decimalAndNegExp
	,leadingZerosAndNegExp
	,lparen
	,rparen
	,lbracket
	,rbracket
	,comma
	,equals
	,semicolon]

plusOP = ("+", [opPosTok "+"])

minusOP = ("-", [opPosTok "-"])

timesOP = ("*", [opPosTok "*"])

invertOp = ("!", [opPosTok "!"])

transposeOp = ("'", [opPosTok "'"])

scalarTimesOP = (".*", [opPosTok ".*"])

varName = ("A", [identifierPosTok "A"])

longVarName = ("A12eirntDue232", [identifierPosTok "A12eirntDue232"])

digitsOnlyNum = ("123", [floatPosTok $ (read :: String -> Float) "123"])

digitsAndExpNum = ("123e3", [floatPosTok 123e3])

digitsAndUpperExpNum = ("34E45", [floatPosTok 34E45])

decimalNum = ("3.1415", [floatPosTok 3.1415])

decimalAndExp = ("3.14159E23", [floatPosTok 3.14159E23])

decimalAndNegExp = ("3.14159e-4", [floatPosTok 3.14159e-4])

leadingZerosAndNegExp = ("00000.000312E-2", [floatPosTok 0.000312E-2])

lparen = ("(", [lparenPosTok])

rparen = (")", [rparenPosTok])

lbracket = ("[", [lbracketPosTok])

rbracket = ("]", [rbracketPosTok])

comma = (",", [commaPosTok])

equals = ("=", [equalsPosTok])

semicolon = (";", [semicolonPosTok])