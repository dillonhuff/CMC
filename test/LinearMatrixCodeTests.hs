module LinearMatrixCodeTests(
	linearMatrixCodeTests) where

import LinearMatrixCode
import ScalarLoopCode
import TestUtils

linearMatrixCodeTests = testFunction scalarLoopFunction testCases

testCases =
	[]