module LinearMatrixCodeTests(
	linearMatrixCodeTests) where

import DataProperties
import LinearMatrixCode
import ScalarLoopCode
import TestUtils

linearMatrixCodeTests = testFunction scalarLoopFunction testCases

testCases =
	[oneArgDec
	,twoArgDec
	,oneRetDec
	,twoRetDec]

oneArgDec =
	(matCodeFunction "" [(genD "X" (genGeneral "a" "b"))] [] []
	,scalarLoopCode "" [gmDec "X" "a" "b"] [] [])

twoArgDec =
	(matCodeFunction "" [defD "X" [1, 2] (defColVec 2), genD "U" (genUpperTriangular "A")] [] []
	,scalarLoopCode "" [cDec "X" "2", gmDec "U" "A" "A"] [] [])

oneRetDec =
	(matCodeFunction "" [] [] [(genD "X" (genGeneral "a" "b"))]
	,scalarLoopCode "" [] [] [gmDec "X" "a" "b"])

twoRetDec =
	(matCodeFunction "" [] [] [defD "X" [1, 2] (defColVec 2), genD "U" (genUpperTriangular "A")]
	,scalarLoopCode "" [] [] [cDec "X" "2", gmDec "U" "A" "A"])