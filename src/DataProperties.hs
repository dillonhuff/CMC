module DataProperties(
	Shape) where

-- This module encodes data about special properties
-- that matrices may have, like shape (vector, matrix, upper triangular...)

data Shape
	= RowVector Dimension
	| ColVector Dimension
	| UpperTriangular Dimension Dimension
	| LowerTriangular Dimension Dimension
	| Symmetric Dimension Dimension
	| General Dimension Dimension
	deriving (Eq, Show)

data Dimension
	= GenericDim String
	| NumberDim Int
	deriving (Eq, Show)