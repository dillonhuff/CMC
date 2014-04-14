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
	,leadingZerosAndNegExp]

plusOP = ("+", [opTok "+"])

minusOP = ("-", [opTok "-"])

timesOP = ("*", [opTok "*"])

invertOp = ("!", [opTok "!"])

transposeOp = ("'", [opTok "'"])

scalarTimesOP = (".*", [opTok ".*"])

varName = ("A", [varTok "A"])

longVarName = ("A12eirntDue232", [varTok "A12eirntDue232"])

digitsOnlyNum = ("123", [floatTok $ (read :: String -> Float) "123"])

digitsAndExpNum = ("123e3", [floatTok 123e3])

digitsAndUpperExpNum = ("34E45", [floatTok 34E45])

decimalNum = ("3.1415", [floatTok 3.1415])

decimalAndExp = ("3.14159E23", [floatTok 3.14159E23])

decimalAndNegExp = ("3.14159e-4", [floatTok 3.14159e-4])

leadingZerosAndNegExp = ("00000.000312E-2", [floatTok 0.000312E-2])