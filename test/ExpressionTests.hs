module ExpressionTests(
	expressionTests) where

import AnnotatedFunction
import Control.Monad
import DataProperties
import ErrorHandling
import Expression
import Parser
import TestUtils
import TypeSystem

expressionTests = do
	testFunction (extractValue . (liftM fst) . (typeOfExpr [])) exprTypeCases
	testFunction (extractValue . (liftM fst) . typeOfExpr testVars) exprWithVarsCases
	testFunction (extractValue . ((=<<) checkFunctionTypes) . parseFunction) functionTypeCases
	testFunction strToExprTrees funcAnnotationCases

strToExprTrees = (extractValue . ((=<<) annotateFunc) . parseFunction)

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
	,(identifier "K", genMatrix "K-row" "K-col")
	,(identifier "C", genMatrix "C-row" "C-col")]

exprWithVarsCases =
	[oneId
	,oneIdDefDimensions
	,addTwo
	,multiplyTwoGen
	,transposeMultExpr]

functionTypeCases =
	[noArgFunc
	,oneArgFunc
	,oneArgArith
	,genericMultiply
	,multipleStatements
	,transposeFunc
	,transposeMult
	,transposeMultDef
	,simpleWithUpperTriangular
	,simpleWithLowerTriangular
	,simpleWithSymmetric
	,simpleWithRowVector
	,transposeRowVector
	,simpleColVector
	,transposeColVector
	,twoScalarTimes]

funcAnnotationCases =
	[oneScalarArg
	,noArgColVec
	,noArgRowVec
	,noArgScalar
	,unaryOpUpperTriangular
	,unaryOpUpperTriangular
	,binaryMultUpperTriangularColVec]

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

transposeMultExpr =
	(binaryOp (operator "*") (unaryOp (operator "'") (identifier "A")) (identifier "K"),
		genMatrix "A-col" "K-col")

noArgFunc = ("func oh() A = [1 2; 1.0 2.3e4]; return(A)", [(identifier "A", defMatrix 2 2)])

oneArgFunc = ("func testT(C) X = C; return(X)",
	[(identifier "X", genMatrix "C-row" "C-col")
	,(identifier "C", genMatrix "C-row" "C-col")])

oneArgArith = ("func testD(U) X = U + [1; 2]; return (X, U)",
	[(identifier "X", defMatrix 2 1)
	,(identifier "U", defMatrix 2 1)])

genericMultiply = ("func testT(X, Y) Q = X * Y; return(Q)",
	[(identifier "Q", genMatrix "X-row" "Y-col")
	,(identifier "X", genMatrix "X-row" "Y-row")
	,(identifier "Y", genMatrix "Y-row" "Y-col")])

multipleStatements = ("func noWay(A, B, C, T) K = A + B; K2 = A + A; K3 = T .* K2 * C; return(K2, K3)",
	[(identifier "K3", genMatrix "B-row" "C-col")
	,(identifier "K2", genMatrix "B-row" "C-row")
	,(identifier "K", genMatrix "B-row" "C-row")
	,(identifier "A", genMatrix "B-row" "C-row")
	,(identifier "B", genMatrix "B-row" "C-row")
	,(identifier "C", genMatrix "C-row" "C-col")
	,(identifier "T", defMatrix 1 1)])

transposeFunc = ("func tp(A) G = A'; return(G)",
	[(identifier "G", genMatrix "A-col" "A-row")
	,(identifier "A", genMatrix "A-row" "A-col")])

transposeMultDef = ("func tpMul() Top = [1 2; 2 3; 4 5]' * [1 2; 2 3; 3 4]; return(Top)",
	[(identifier "Top", defMatrix 2 2)])

transposeMult = ("func tpMul(A, B) Res = A' * B; return(Res)",
	[(identifier "Res", genMatrix "A-col" "B-col")
	,(identifier "A", genMatrix "B-row" "A-col")
	,(identifier "B", genMatrix "B-row" "B-col")])

simpleWithUpperTriangular = ("func spec([UpperTriangular] G) X = G; return(X)",
	[(identifier "X", genMatrix "G-row" "G-row")
	,(identifier "G", genMatrix "G-row" "G-row")])

simpleWithLowerTriangular = ("func spec([LowerTriangular] G) X = G; return(X)",
	[(identifier "X", genMatrix "G-row" "G-row")
	,(identifier "G", genMatrix "G-row" "G-row")])

simpleWithSymmetric = ("func spec([Symmetric] G) X = G'; return(X)",
	[(identifier "X", genMatrix "G-row" "G-row")
	,(identifier "G", genMatrix "G-row" "G-row")])

simpleWithRowVector = ("func rv([RowVector] X) Ju = X; return(Ju)",
	[(identifier "Ju", leftDefMatrix 1 "X-col")
	,(identifier "X", leftDefMatrix 1 "X-col")])

transposeRowVector = ("func rv([RowVector] X) Ju = X'; return(Ju)",
	[(identifier "Ju", rightDefMatrix "X-col" 1)
	,(identifier "X", leftDefMatrix 1 "X-col")])

simpleColVector = ("func rv([ColumnVector] L) Ju = L; return(Ju)",
	[(identifier "Ju", rightDefMatrix "L-row" 1)
	,(identifier "L", rightDefMatrix "L-row" 1)])

transposeColVector = ("func rv([ColumnVector] L) Ju = L'; return(Ju)",
	[(identifier "Ju", leftDefMatrix 1 "L-row")
	,(identifier "L", rightDefMatrix "L-row" 1)])

twoScalarTimes = ("func tsa([Scalar] G, X, [Scalar] Y) K = G * Y; return(K)",
	[(identifier "K", defMatrix 1 1)
	,(identifier "G", defMatrix 1 1)
	,(identifier "X", genMatrix "X-row" "X-col")
	,(identifier "Y", defMatrix 1 1)])

oneScalarArg = ("func oneScalar([Scalar] P) X = P; return(X)",
	annotatedFunction
		"oneScalar"
		[aId "P" scalar]
		[aBinop "=" (aId "X" scalar) (aId "P" scalar) scalar]
		[aId "X" scalar])

noArgColVec = ("func oneMat() L = [1.2; 3.4]; return(L)",
	annotatedFunction
		"oneMat"
		[]
		[aBinop "=" (aId "L" (defColVec 2)) (aMat [1.2, 3.4] (defColVec 2)) (defColVec 2)]
		[aId "L" (defColVec 2)])

noArgRowVec = ("func oneMat() L = [1.2 3.4]; return(L)",
	annotatedFunction
		"oneMat"
		[]
		[aBinop "=" (aId "L" (defRowVec 2)) (aMat [1.2, 3.4] (defRowVec 2)) (defRowVec 2)]
		[aId "L" (defRowVec 2)])

noArgScalar = ("func oneMat() L = [-1.2e-3]; return(L)",
	annotatedFunction
		"oneMat"
		[]
		[aBinop "=" (aId "L" scalar) (aMat [-1.2e-3] scalar) scalar]
		[aId "L" scalar])

unaryOpUpperTriangular = ("func upT([UpperTriangular] T) X = T'; return(X)",
	annotatedFunction
		"upT"
		[aId "T" (genUpperTriangular "T-row")]
		[aBinop
			"="
			(aId "X" (genLowerTriangular "T-row"))
			(aUnop "'" (aId "T" (genUpperTriangular "T-row")) (genLowerTriangular "T-row"))
			(genLowerTriangular ("T-row"))]
		[aId "X" (genLowerTriangular "T-row")])

unaryOpLowerTriangular = ("func upT([LowerTriangular] T) X = T'; return(X)",
	annotatedFunction
		"upT"
		[aId "T" (genLowerTriangular "T-row")]
		[aBinop
			"="
			(aId "X" (genUpperTriangular "T-row"))
			(aUnop "'" (aId "T" (genLowerTriangular "T-row")) (genUpperTriangular "T-row"))
			(genUpperTriangular ("T-row"))]
		[aId "X" (genLowerTriangular "T-row")])

binaryMultUpperTriangularColVec =
	("func btr([UpperTriangular] U, [ColumnVector] R) K = U * R; return(K)",
	annotatedFunction
		"btr"
		[aId "U" (genUpperTriangular "R-row"), aId "R" (genColVec "R-row")]
		[aBinop
			"="
			(aId "K" (genColVec "R-row"))
			(aBinop
				"*"
				(aId "U" (genUpperTriangular "R-row"))
				(aId "R" (genColVec "R-row"))
				(genColVec "R-row"))
			(genColVec "R-row")]
		[aId "K" (genColVec "R-row")])