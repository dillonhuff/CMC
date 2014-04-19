module LinearMatrixCodeTests(
	linearMatrixCodeTests) where

import LinearMatrixCode
import ScalarLoopCode
import TestUtils

linearMatrixCodeTests = testFunction matInstrToScalarCode testCases

testCases =
	[]