module TypeSystemTests() where

import ErrorHandling
import TestUtils
import TypeSystem

typeSystemTests = do
	typeSystemCorrectTests

-- Tests where the input constraints have a solution
typeSystemCorrectTests =
	testFunction (extractValue . computeType) correctlyTypedCases

correctlyTypedCases =
	[]