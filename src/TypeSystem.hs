module TypeSystem(
	computeType) where

import ErrorHandling

data Type
	= DefMatrix Int Int
	deriving (Eq)

instance Show Type where
	show = showType

showType :: Type -> String
showType (DefMatrix r c) = "M " ++ show r ++ " " ++show c

type TypeConstraint = (Type, Type)

computeType :: [TypeConstraint] -> Error Type
computeType constraints = Succeeded $ DefMatrix 1 1