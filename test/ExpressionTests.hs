module ExpressionTests(
	expressionTests) where

import ErrorHandling
import Expression
import Parser
import TestUtils
import TypeSystem

expressionTests = do
	testFunction (extractValue . (typeOfExpr [])) exprTypeCases
	testFunction (extractValue . ((=<<) checkFunctionTypes) . parseFunction) functionTypeCases

exprTypeCases =
	[oneMatrix
	,negateOp
	,transposeOp
	,inverseOp
	,plusOp
	,minusOp
	,timesOp
	,scalarTimesOp]

functionTypeCases =
	[noArgFunction]

oneMatrix = ((matrix 2 4 [1, 2, 3, 4, 5, 6, 7, 8]), defMatrix 2 4)

negateOp = (unaryOp (operator "-") (matrix 1 1 [2.0]), defMatrix 1 1)

transposeOp = (unaryOp (operator "'") (matrix 7 9 [2.0]), defMatrix 9 7)

inverseOp = (unaryOp (operator "!") (matrix 8 8 []), defMatrix 8 8)

plusOp = (binaryOp (operator "+") (matrix 1 4 []) (matrix 1 4 []), defMatrix 1 4)

minusOp = (binaryOp (operator "-") (matrix 3 289 []) (matrix 3 289 []), defMatrix 3 289)

timesOp = (binaryOp (operator "*") (matrix 38 29 []) (matrix 29 256 []), defMatrix 38 256)

scalarTimesOp = (binaryOp (operator ".*") (matrix 1 1 [3.4]) (matrix 6 71 []), defMatrix 6 71)

noArgFunction = ("func ohNo() X = [1.2]; return(X)", [defMatrix 1 1])