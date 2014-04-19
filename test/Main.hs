module Main(main) where

import AnnotatedFunctionTests
import DataFlowGraphTests
import DataPropertiesTests
import ExpressionTests
import LexerTests
import LinearMatrixCodeTests
import ParserTests
import TypeSystemTests

main = do
	lexerTests
	parserTests
	typeSystemTests
	expressionTests
	dataFlowGraphTests
	dataPropertiesTests
	annotatedFunctionTests
	linearMatrixCodeTests