module Main(main) where

import LexerTests
import ParserTests

main = do
	lexerTests
	parserTests
	typeSystemTests
	expressionTests