module ScalarLoopCode(
	ScalarLoopFunction,
	scalarLoopCode) where

import DataProperties

data ScalarLoopFunction = SCF String [SData] [Loop] [SData]
	deriving (Eq, Show)

data Loop = NONE
	deriving (Eq, Show)

data SData
	= SGen String Shape
	| SDef String [Float] Shape
	deriving (Eq, Show)

scalarLoopCode :: String -> [SData] -> [Loop] -> [SData] -> ScalarLoopFunction
scalarLoopCode name args body retVals = SCF name args body retVals