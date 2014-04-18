module DataPropertiesTests(
	dataPropertiesTests) where

import DataProperties
import TestUtils

dataPropertiesTests = do
	testFunction binopResultShapeTuple binopTestCases
	testFunction unopResultShapeTuple unopTestCases

binopResultShapeTuple :: (String, Shape, Shape) -> Shape
binopResultShapeTuple (opName, s1, s2) = binopResultShape opName s1 s2

unopResultShapeTuple :: (String, Shape) -> Shape
unopResultShapeTuple (opName, s) = unopResultShape opName s

binopTestCases =
	[(("+", scalar, scalar), scalar)
	,(("-", scalar, scalar), scalar)
	,(("*", scalar, scalar), scalar)
	,((".*", scalar, scalar), scalar)
	,(("+", genGeneral "A" "B", genGeneral "A" "B"), genGeneral "A" "B")
	,(("-", genGeneral "A" "B", genGeneral "A" "B"), genGeneral "A" "B")
	,(("*", genGeneral "A" "B", genGeneral "A" "B"), genGeneral "A" "B")
	,((".*", scalar, genGeneral "A" "G"), genGeneral "A" "G")
	,(("+", genUpperTriangular "A", genUpperTriangular "A"), genUpperTriangular "A")
	,(("-", genUpperTriangular "A", genUpperTriangular "A"), genUpperTriangular "A")
	,(("*", genUpperTriangular "A", genUpperTriangular "A"), genUpperTriangular "A")
	,((".*", scalar, genUpperTriangular "A"), genUpperTriangular "A")
	,(("+", genLowerTriangular "A", genLowerTriangular "A"), genLowerTriangular "A")
	,(("-", genLowerTriangular "A", genLowerTriangular "A"), genLowerTriangular "A")
	,(("*", genLowerTriangular "A", genLowerTriangular "A"), genLowerTriangular "A")
	,((".*", scalar, genLowerTriangular "A"), genLowerTriangular "A")]

unopTestCases =
	[(("'", scalar), scalar)
	,(("'", defRowVec 45), defColVec 45)
	,(("'", defColVec 2), defRowVec 2)
	,(("'", genSymmetric "A"), genSymmetric "A")
	,(("'", genUpperTriangular "G"), genLowerTriangular "G")
	,(("'", genLowerTriangular "G"), genUpperTriangular "G")
	,(("'", genGeneral "T" "K"), genGeneral "T" "K")]