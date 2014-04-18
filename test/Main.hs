module Main(main) where

import DataFlowGraphTests
import DataPropertiesTests
import ExpressionTests
import LexerTests
import ParserTests
import TypeSystemTests

main = do
	lexerTests
	parserTests
	typeSystemTests
	expressionTests
	dataFlowGraphTests
	dataPropertiesTests