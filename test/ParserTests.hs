module ParserTests(
	parserTests) where

import ErrorHandling
import Expression
import Parser
import TestUtils

parserTests = do
	testFunction (extractValue . parseAssignments) parseCases
	testFunction (extractValue . parseFunction) parseFuncCases

parseCases =
	[scalarAssign
	,scalarAssignExp
	,idAssign
	,matrixDeclaration
	,negationAssign
	,inverseAssign
	,transposeAssign
	,addAssign
	,subAssign
	,timesAssign
	,scalarTimesAssign
	,multiAssign]

parseFuncCases =
	[simpleFunc
	,multiReturnFunc
	,specialShapeFunc
	,multiArgShapesFunc
	,noDefaultShapeForSomeArgs]

scalarAssign = ("A = 4;", [assign (identifier "A") (float 4)])

scalarAssignExp = ("S = 23.3e-12;", [assign (identifier "S") (float 23.3e-12)])

idAssign = ("A = B;", [assign (identifier "A") (identifier "B")])

matrixDeclaration = ("G12 = [1 3; 3.4 4.5; -4.5 0.9e9];",
	[assign (identifier "G12")
		(matrix 3 2 [1, 3, 3.4, 4.5, -4.5, 0.9e9])])

negationAssign = ("R3 = - R3;",
	[assign (identifier "R3")
		(unaryOp
			(operator "-")
			(identifier "R3"))])

inverseAssign = ("G = T!;",
	[assign (identifier "G")
		(unaryOp 
			(operator "!")
			(identifier "T"))])

transposeAssign = ("G = T';",
	[assign (identifier "G")
		(unaryOp 
			(operator "'")
			(identifier "T"))])

addAssign = ("K = W + [1 2.3; -3.4 4];",
	[assign (identifier "K")
		(binaryOp
			(operator "+")
			(identifier "W")
			(matrix 2 2 [1, 2.3, -3.4, 4]))])

subAssign = ("K = [1 2.3; -3.4 4; 9.3e3 9.0] - Z34A;",
	[assign (identifier "K")
		(binaryOp
			(operator "-")
			(matrix 3 2 [1, 2.3, -3.4, 4, 9.3e3, 9.0])
			(identifier "Z34A"))])

timesAssign = ("K = Q * Z34A;",
	[assign (identifier "K")
		(binaryOp
			(operator "*")
			(identifier "Q")
			(identifier "Z34A"))])

scalarTimesAssign = ("K = -34.4e3 .* Z34A;",
	[assign (identifier "K")
		(binaryOp
			(operator ".*")
			(unaryOp (operator "-") (float 34.4e3))
			(identifier "Z34A"))])

multiAssign = ("S = [1 2.3; 3 4]; K = -(S + [-9 -9.8E-3; 3 7]);",
	[assign (identifier "S") (matrix 2 2 [1, 2.3, 3, 4])
	,assign (identifier "K") (unaryOp
		(operator "-")
		(binaryOp
			(operator "+")
			(identifier "S")
			(matrix 2 2 [-9, -9.8e-3, 3, 7])))])

simpleFunc = ("func noWay(A, B) A = B; return(A)",
	function
		(funcall "noWay")
		[identifier "A", identifier "B"]
		[assign (identifier "A") (identifier "B")]
		[identifier "A"])

multiReturnFunc = ("func ohNo12(C) C = [1 2; 3 4]; return(A, B, C)",
	function
		(funcall "ohNo12")
		[identifier "C"]
		[assign (identifier "C") (matrix 2 2 [1, 2, 3, 4])]
		[identifier "A", identifier "B", identifier "C"])

specialShapeFunc = ("func sp([UpperTriangular] A) X = A; return(X)",
	functionSpec
		(funcall "sp")
		[[identifier "UpperTriangular"]]
		[identifier "A"]
		[assign (identifier "X") (identifier "A")]
		[identifier "X"])

multiArgShapesFunc =
	("func nope([General] G, [Symmetric] M, [LowerTriangular] H) K = M + H; J = G'; return(K, J)",
		functionSpec
			(funcall "nope")
			[[identifier "General"], [identifier "Symmetric"], [identifier "LowerTriangular"]]
			[identifier "G", identifier "M", identifier "H"]
			[assign (identifier "K")
				(binaryOp
					(operator "+")
					(identifier "M")
					(identifier "H"))
			,assign (identifier "J")
				(unaryOp
					(operator "'")
					(identifier "G"))]
			[identifier "K", identifier "J"])

noDefaultShapeForSomeArgs =
	("func tre([Symmetric] U, H) X = U - H; return(X)",
		functionSpec
			(funcall "tre")
			[[identifier "Symmetric"], []]
			[identifier "U", identifier "H"]
			[assign (identifier "X")
				(binaryOp
					(operator "-")
					(identifier "U")
					(identifier "H"))]
			[identifier "X"])