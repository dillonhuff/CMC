module ParserTests() where

import ErrorHandling
import Expression
import Parser
import TestUtils

parserTests = do
	testFunction (extractValue . parseAssignment) parseCases

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
	,scalarTimesAssign]

scalarAssign = ("A = 4;", assign (identifier "A") (float 4))

scalarAssignExp = ("S = 23.3e-12;", assign (identifier "S") (float 23.3e-12))

idAssign = ("A = B;", assign (identifier "A") (identifier "B"))

matrixDeclaration = ("G12 = [1 3; 3.4 4.5; -4.5 0.9e9];",
	assign (identifier "G12")
		(matrix 3 2 [1, 3, 3.4, 4.5, -4.5, 0.9e9]))

negationAssign = ("R3 = - R3;",
	assign (identifier "R3")
		(unaryOp
			(operator "-")
			(identifier "R3")))

inverseAssign = ("G = T!;",
	assign (identifier "G")
		(unaryOp 
			(operator "!")
			(identifier "T")))

transposeAssign = ("G = T';",
	assign (identifier "G")
		(unaryOp 
			(operator "'")
			(identifier "T")))

addAssign = ("K = W + [1 2.3; -3.4 4];",
	assign (identifier "K")
		(binaryOp
			(operator "+")
			(identifier "W")
			(matrix 2 2 [1, 2.3, -3.4, 4])))

subAssign = ("K = [1 2.3; -3.4 4; 9.3e3 9.0] - Z34A;",
	assign (identifier "K")
		(binaryOp
			(operator "-")
			(matrix 3 2 [1, 2.3, -3.4, 4, 9.3e3, 9.0])
			(identifier "Z34A")))

timesAssign = ("K = Q * Z34A;",
	assign (identifier "K")
		(binaryOp
			(operator "*")
			(identifier "Q")
			(identifier "Z34A")))

scalarTimesAssign = ("K = -34.4e3 .* Z34A;",
	assign (identifier "K")
		(binaryOp
			(operator ".*")
			(unaryOp (operator "-") (float 34.4e3))
			(identifier "Z34A")))