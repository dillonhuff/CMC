module ParserTests() where

import ErrorHandling
import Expression
import Parser
import TestUtils

parserTests = do
	testFunction (extractValue . parseAssignment) parseCases

parseCases =
	[scalarAssign
	,idAssign
	,matrixDeclaration]

scalarAssign = ("A = 4;", assign (identifier "A") (float 4))

idAssign = ("A = B;", assign (identifier "A") (identifier "B"))

matrixDeclaration = ("G12 = [1 3; 3.4 4.5; -4.5 0.9e9];",
	assign (identifier "G12")
		(matrix 3 2 [1, 3, 3.4, 4.5, -4.5, 0.9e9]))