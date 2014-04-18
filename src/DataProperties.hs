module DataProperties(
	Shape, genNum, scalar, makeShape,
	defColVec, defRowVec, dimsToShape,
	binopResultShape, unopResultShape,
	genUpperTriangular, genLowerTriangular,
	genGeneral, genSymmetric) where

import TypeSystem

-- This module encodes data about special properties
-- that matrices may have, like shape (vector, matrix, upper triangular...)

data Shape
	= Scalar
	| RowVector Dimension
	| ColVector Dimension
	| UpperTriangular Dimension
	| LowerTriangular Dimension
	| Symmetric Dimension
	| General Dimension Dimension
	deriving (Eq, Show)

rowDim :: Shape -> Dimension
rowDim Scalar = NumberDim 1
rowDim (RowVector _) = NumberDim 1
rowDim (ColVector d) = d
rowDim (UpperTriangular d) = d
rowDim (LowerTriangular d) = d
rowDim (Symmetric d) = d
rowDim (General r c) = r

colDim :: Shape -> Dimension
colDim (General r c) = c
colDim (RowVector d) = d
colDim (ColVector _) = NumberDim 1
colDim s = rowDim s

binopResultShape :: String -> Shape -> Shape -> Shape
binopResultShape opName s1 s2 = case opName of
	"+" -> plusRes s1 s2
	"-" -> minusRes s1 s2
	"*" -> timesRes s1 s2
	".*" -> s2

plusRes (General a b) _ = General a b
plusRes _ (General a b) = General a b
plusRes s1 s2 = s1

minusRes (General a b) _ = General a b
minusRes _ (General a b) = General a b
minusRes s1 s2 = s1

timesRes (General a b) (General c d) = General a d
timesRes (Symmetric a) (Symmetric b) = General a a
timesRes (UpperTriangular d) (UpperTriangular e) = UpperTriangular d
timesRes (LowerTriangular d) (LowerTriangular e) = LowerTriangular d
timesRes (RowVector d) (ColVector e) = General d e
timesRes (ColVector d) (RowVector e) = Scalar
timesRes (RowVector a) (General b c) = RowVector c
timesRes (General a b) (ColVector c) = ColVector b
timesRes Scalar Scalar = Scalar
timesRes (General a b) s = General a (colDim s)
timesRes s (General c d) = General (rowDim s) d


unopResultShape :: String -> Shape -> Shape
unopResultShape "'" (General r c) = General r c
unopResultShape "'" (UpperTriangular d) = LowerTriangular d
unopResultShape "'" (LowerTriangular d) = UpperTriangular d
unopResultShape "'" (RowVector d) = ColVector d
unopResultShape "'" (ColVector d) = RowVector d
unopResultShape "'" (Symmetric d) = Symmetric d
unopResultShape "'" Scalar = scalar

dimsToShape :: Int -> Int -> Shape
dimsToShape 1 1 = Scalar
dimsToShape r 1 = ColVector (NumberDim r)
dimsToShape 1 r	= RowVector (NumberDim r)
dimsToShape r c = General (NumberDim r) (NumberDim c)

shapeNames =
	["UpperTriangular", "LowerTriangular"
	,"Symmetric", "RowVector", "ColumnVector"
	,"Scalar", "General"]

-- Takes a list of names of properties and a type and returns
-- the matching shape
makeShape :: [String] -> Type -> Shape
makeShape propNames t = Scalar

data Dimension
	= GenericDim String
	| NumberDim Int
	deriving (Eq, Show)

scalar = Scalar
defColVec d = ColVector (NumberDim d)
defRowVec d = RowVector (NumberDim d)
genSymmetric d = Symmetric (GenericDim d)
genUpperTriangular d = UpperTriangular (GenericDim d)
genLowerTriangular d = LowerTriangular (GenericDim d)
genGeneral r c = General (GenericDim r) (GenericDim c)
genNum r c = General (NumberDim r) (NumberDim c)