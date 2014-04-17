module ExpressionTests(
	expressionTests) where

import Control.Monad
import ErrorHandling
import Expression
import Parser
import TestUtils
import TypeSystem

expressionTests = do
	testFunction (extractValue . (liftM fst) . (typeOfExpr [])) exprTypeCases
	testFunction (extractValue . (liftM fst) . typeOfExpr testVars) exprWithVarsCases
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

testVars =
	[(identifier "A", genMatrix "A-row" "A-col")
	,(identifier "B", defMatrix 1 2)
	,(identifier "C", genMatrix "C-row" "C-col")]

exprWithVarsCases =
	[oneId
	,oneIdDefDimensions
	,addTwo
	,multiplyTwoGen]

functionTypeCases =
	[noArgFunc
	,oneArgFunc
	,oneArgArith]

oneMatrix = ((matrix 2 4 [1, 2, 3, 4, 5, 6, 7, 8]), defMatrix 2 4)

negateOp = (unaryOp (operator "-") (matrix 1 1 [2.0]), defMatrix 1 1)

transposeOp = (unaryOp (operator "'") (matrix 7 9 [2.0]), defMatrix 9 7)

inverseOp = (unaryOp (operator "!") (matrix 8 8 []), defMatrix 8 8)

plusOp = (binaryOp (operator "+") (matrix 1 4 []) (matrix 1 4 []), defMatrix 1 4)

minusOp = (binaryOp (operator "-") (matrix 3 289 []) (matrix 3 289 []), defMatrix 3 289)

timesOp = (binaryOp (operator "*") (matrix 38 29 []) (matrix 29 256 []), defMatrix 38 256)

scalarTimesOp = (binaryOp (operator ".*") (matrix 1 1 [3.4]) (matrix 6 71 []), defMatrix 6 71)

oneId = (identifier "A", genMatrix "A-row" "A-col")

addTwo = (binaryOp (operator "+") (identifier "A") (identifier "B"), defMatrix 1 2)

oneIdDefDimensions = (identifier "B", defMatrix 1 2)

multiplyTwoGen = (binaryOp (operator "*") (identifier "A") (identifier "C"), genMatrix "A-row" "C-col")

noArgFunc = ("func oh() A = [1 2; 1.0 2.3e4]; return(A)", [(identifier "A", defMatrix 2 2)])

oneArgFunc = ("func testT(C) X = C; return(X)",
	[(identifier "X", genMatrix "C-row" "C-col")
	,(identifier "C", genMatrix "C-row" "C-col")])

oneArgArith = ("func testD(U) X = U + [1; 2]; return (X, U)",
	[(identifier "X", defMatrix 2 1)
	,(identifier "U", defMatrix 2 1)])