module DataProperties(
	Shape) where

-- This module encodes data about special properties
-- that matrices may have, like shape (vector, matrix, upper triangular...)

data Shape
	= RowVector Int
	| ColVector Int
	| UpperTriangular Int Int
	| LowerTriangular Int Int
	| Symmetric Int Int
	| General Int Int
	deriving (Eq, Show)


