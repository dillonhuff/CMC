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
	[scalar
	,reverseVar
	,matSubBothVars
	,unaryOp
	,matSubSplitDims]

scalar = ([(typeVar "t0", defMatrix 2 1)], defMatrix 2 1)

reverseVar = ([(defMatrix 12 23, typeVar "t0")], defMatrix 12 23)

matSubBothVars = ([(typeVar "t0", genMatrix "a" "b"), (genMatrix "a" "b", defMatrix 2 5)], defMatrix 2 5)

matSubSplitDims = ([(typeVar "t0", genMatrix "a" "b"),
					(genMatrix "a" "b", leftDefMatrix 3 "c"),
					(genMatrix "a" "b", rightDefMatrix "q" 4)],
					defMatrix 3 4)

unaryOp = ([(func (genMatrix "a" "b") (genMatrix "a" "b"),
				(func (typeVar "t00") (typeVar "t0"))),
			(typeVar "t00", defMatrix 2 7)], defMatrix 2 7)