module Main(main) where

import ExpressionTests
import LexerTests
import ParserTests
import TypeSystemTests

main = do
	lexerTests
	parserTests
	typeSystemTests
	expressionTests