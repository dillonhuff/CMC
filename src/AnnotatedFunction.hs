module AnnotatedFunction(
	AnnotatedFunction, AExpr, aId, aBinop, aUnop, aMat,
	shapeOf, getIdShape, nameOf,
	annotatedFunction) where

import DataProperties

annotatedFunction :: String -> [AExpr] -> [AExpr] -> [AExpr] -> AnnotatedFunction
annotatedFunction name args body retVals = AF name args body retVals

data AnnotatedFunction = AF String [AExpr] [AExpr] [AExpr]
	deriving (Eq, Show)

data AExpr
	= AMat [Float] Shape
	| AIdent String Shape
	| ABinop String AExpr AExpr Shape
	| AUnop String AExpr Shape
	deriving (Eq, Show)

shapeOf :: AExpr -> Shape
shapeOf (AMat _ s) = s
shapeOf (AIdent _ s) = s
shapeOf (ABinop _ _ _ s) = s
shapeOf (AUnop _ _ s) = s

nameOf :: AExpr -> String
nameOf (AIdent n _) = n
nameOf (ABinop n _ _ _) = n
nameOf (AUnop n _ _) = n

-- TODO find faster way of doing this if it becomes a
-- performance issue
getIdShape :: String -> [AExpr] -> Maybe Shape
getIdShape _ [] = Nothing
getIdShape n (AIdent idName s:rest) = if n == idName
	then Just s
	else getIdShape n rest

aMat vals shape = AMat vals shape
aId name shape = AIdent name shape
aBinop name arg1 arg2 shape = ABinop name arg1 arg2 shape
aUnop name arg shape = AUnop name arg shape