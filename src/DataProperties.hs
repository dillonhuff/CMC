module DataProperties(
	Shape, defGeneral, scalar, makeShape,
	defColVec, defRowVec, dimsToShape,
	binopResultShape, unopResultShape,
	genUpperTriangular, genLowerTriangular,
	genGeneral, genSymmetric, genColVec,
	genRowVec) where

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

minusRes (General a b) u_ = General a b
minusRes _ (General a b) = General a b
minusRes s1 s2 = s1

timesRes (General a b) (General c d) = General a d
timesRes (Symmetric a) (Symmetric b) = General a a
timesRes (UpperTriangular d) (UpperTriangular e) = UpperTriangular d
timesRes (LowerTriangular d) (LowerTriangular e) = LowerTriangular d
timesRes (ColVector d) (RowVector e) = General d e
timesRes (RowVector d) (ColVector e) = Scalar
timesRes (RowVector a) (General b c) = RowVector c
timesRes t (ColVector c) = ColVector (rowDim t)
timesRes Scalar Scalar = Scalar
timesRes (General a b) s = General a (colDim s)
timesRes s (General c d) = General (rowDim s) d
timesRes t s = error $ "arg1 = " ++ show t ++ " and arg2 = " ++ show s

unopResultShape :: String -> Shape -> Shape
unopResultShape op s = case op of
	"'" -> transposeResultShape s
	"!" -> inverseResultShape s
	"-" -> s

transposeResultShape :: Shape -> Shape
transposeResultShape (General r c) = General r c
transposeResultShape (UpperTriangular d) = LowerTriangular d
transposeResultShape (LowerTriangular d) = UpperTriangular d
transposeResultShape (RowVector d) = ColVector d
transposeResultShape (ColVector d) = RowVector d
transposeResultShape (Symmetric d) = Symmetric d
transposeResultShape Scalar = scalar

inverseResultShape (General r c) = General r r
inverseResultShape (UpperTriangular d) = UpperTriangular d
inverseResultShape (LowerTriangular d) = LowerTriangular d
inverseResultShape (Symmetric d) = Symmetric d
inverseResultShape Scalar = Scalar

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
makeShape propNames t = case getShapeName propNames of
	"UpperTriangular" -> UpperTriangular (makeDimension $ rowDimension t)
	"LowerTriangular" -> LowerTriangular (makeDimension $ rowDimension t)
	"ColumnVector" -> ColVector (makeDimension $ rowDimension t)
	"RowVector" -> RowVector (makeDimension $ colDimension t)
	"Symmetric" -> Symmetric (makeDimension $ rowDimension $ t)
	"Scalar" -> Scalar
	_ -> General (makeDimension $ rowDimension t) (makeDimension $ colDimension t)

getShapeName propNames = head $ filter (\x -> elem x shapeNames) propNames

makeDimension :: Type -> Dimension
makeDimension t = if isTypeVar t
	then GenericDim $ varName t
	else NumberDim $ dimNum t

data Dimension
	= GenericDim String
	| NumberDim Int
	deriving (Eq, Show)

scalar = Scalar
defColVec d = ColVector (NumberDim d)
defRowVec d = RowVector (NumberDim d)
defGeneral r c = General (NumberDim r) (NumberDim c)
genColVec d = ColVector (GenericDim d)
genRowVec d = RowVector (GenericDim d)
genSymmetric d = Symmetric (GenericDim d)
genUpperTriangular d = UpperTriangular (GenericDim d)
genLowerTriangular d = LowerTriangular (GenericDim d)
genGeneral r c = General (GenericDim r) (GenericDim c)