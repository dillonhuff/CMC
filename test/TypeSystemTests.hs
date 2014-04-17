module TypeSystemTests(
	typeSystemTests) where

import ErrorHandling
import TestUtils
import TypeSystem

typeSystemTests = do
	typeSystemCorrectTests

-- Tests where the input constraints have a solution
typeSystemCorrectTests = do
	testFunction (extractValue . computeType) correctlyTypedCases
	testFunction (uniqueTypeNames "t-0") typeNameCases

correctlyTypedCases =
	[identicalDims
	,scalar
	,reverseVar
	,genAndDefMatrix
	,twoGenMatrices
	,reverseGenMat
	,matSubBothVars
	,unaryOp
	,matSubSplitDims
	,transMulMain
	,transMulMain
	,transMulC1C4
	,transMulC4C1
	,transMulDiffOrder
	,transMultiply]

typeNameCases =
	[oneTypeVar
	,funcType
	,matrixType
	,nestedFuncMatrix]

identicalDims = ([(dimension 1, dimension 1), (defMatrix 1 4, typeVar "t-0")], defMatrix 1 4)

scalar = ([(typeVar "t-0", defMatrix 2 1)], defMatrix 2 1)

reverseVar = ([(defMatrix 12 23, typeVar "t-0")], defMatrix 12 23)

genAndDefMatrix = ([(defMatrix 1 6, genMatrix "A" "B"), (genMatrix "A" "B", typeVar "t-0")], defMatrix 1 6)

twoGenMatrices = ([(genMatrix "A" "B", genMatrix "Q" "C"), (genMatrix "A" "B", typeVar "t-0")], genMatrix "Q" "C")

reverseGenMat = ([(genMatrix "b" "c", genMatrix "B-row" "B-col"), (genMatrix "a" "c", typeVar "t-0")],
	genMatrix "a" "B-col")

matSubBothVars = ([(typeVar "t-0", genMatrix "a" "b"), (genMatrix "a" "b", defMatrix 2 5)], defMatrix 2 5)

matSubSplitDims = ([(typeVar "t-0", genMatrix "a" "b"),
					(genMatrix "a" "b", leftDefMatrix 3 "c"),
					(genMatrix "a" "b", rightDefMatrix "q" 4)],
					defMatrix 3 4)

c1 = (func (genMatrix "a" "b") (func (genMatrix "b" "c") (genMatrix "a" "c")),
	func (typeVar "t-01") (func (typeVar "t-02") (typeVar "t-0")))
c2 = (func (genMatrix "d" "e") (genMatrix "e" "d"), func (typeVar "t-010") (typeVar "t-01"))
c3 = (typeVar "t-010", genMatrix "A-row" "A-col")
c4 = (typeVar "t-02", genMatrix "B-row" "B-col")

transMulMain = ([c1], genMatrix "a" "c")

transMulC1C4 = ([c1, c4], genMatrix "a" "B-col")

transMulC4C1 = ([c4, c1], genMatrix "a" "B-col")

transMulDiffOrder = ([c4, c3, c2, c1], genMatrix "A-col" "B-col")

transMultiply = ([c1, c2, c3, c4], genMatrix "A-col" "B-col")

unaryOp = ([(func (genMatrix "a" "b") (genMatrix "a" "b"),
				(func (typeVar "t-00") (typeVar "t-0"))),
			(typeVar "t-00", defMatrix 2 7)], defMatrix 2 7)

oneTypeVar = (typeVar "a", typeVar "t-0-a")

funcType = (func (typeVar "a") (typeVar "b"), func (typeVar "t-01-a") (typeVar "t-02-b"))

matrixType = (genMatrix "tr" "ot", genMatrix "t-01-tr" "t-02-ot")

nestedFuncMatrix = (func (genMatrix "a" "b") (func (genMatrix "a" "b") (genMatrix "a" "b")),
	(func (genMatrix "t-011-a" "t-012-b")
		(func (genMatrix "t-011-a" "t-012-b")
			(genMatrix "t-011-a" "t-012-b"))))