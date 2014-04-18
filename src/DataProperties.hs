module DataProperties(
	Shape, genNum, scalar, makeShape,
	defColVec, defRowVec, dimsToShape) where

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
genNum r c = General (NumberDim r) (NumberDim c)