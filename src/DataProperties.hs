module DataProperties(
	Shape, genNum) where

-- This module encodes data about special properties
-- that matrices may have, like shape (vector, matrix, upper triangular...)

data Shape
	= RowVector Dimension
	| ColVector Dimension
	| UpperTriangular Dimension
	| LowerTriangular Dimension
	| Symmetric Dimension
	| General Dimension Dimension
	deriving (Eq, Show)

data Dimension
	= GenericDim String
	| NumberDim Int
	deriving (Eq, Show)

genNum :: Int -> Int -> Shape
genNum r c = General (NumberDim r) (NumberDim c)