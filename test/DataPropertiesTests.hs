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
	[]

unopTestCases =
	[(("'", scalar), scalar)
	,(("'", defRowVec 45), defColVec 45)
	,(("'", defColVec 2), defRowVec 2)
	,(("'", genSymmetric "A"), genSymmetric "A")
	,(("'", genUpperTriangular "G"), genLowerTriangular "G")
	,(("'", genLowerTriangular "G"), genUpperTriangular "G")
	,(("'", genGeneral "T" "K"), genGeneral "T" "K")]