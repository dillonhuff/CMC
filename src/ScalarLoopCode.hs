module ScalarLoopCode(
	ScalarLoopFunction, Iteration,
	Declaration,
	scalarLoopCode, scalarOp,
	sDec, cDec, rDec, gmDec) where

import DataProperties

scalarLoopCode :: String -> [Declaration] -> [Iteration] -> [Declaration] -> ScalarLoopFunction
scalarLoopCode name args body retVals = SCF name args body retVals

data ScalarLoopFunction = SCF String [Declaration] [Iteration] [Declaration]
	deriving (Eq, Show)

data Iteration
	= Iteration {
		referenced :: [String],
		updated :: [String],
		starts :: [String],
		ends :: [String],
		body :: [Update]}
	deriving (Eq, Show)

scalarOp :: String -> String -> String -> String -> Iteration
scalarOp opName arg1 arg2 res =
	Iteration {
		referenced = [arg1, arg2, res],
		updated = [res],
		starts = [],
		ends = [],
		body = [(SRef res, Binop opName (SRef arg1) (SRef arg2))] }

type Update = (SExpr, SExpr)

-- TODO: Add more specific declarations later. For
-- now all matrices will declare as GeneralMatrix
-- regardless of special properties
data Declaration
	= Scalar String
	| RowVector String String
	| ColVector String String
	| GeneralMatrix String String String
	deriving (Eq, Show)

sDec s = Scalar s
rDec rv d = RowVector rv d
cDec cv d = ColVector cv d
gmDec m r c = GeneralMatrix m r c

data SExpr
	= SRef String
	| VecRef String String
	| MatRef String String String
	| Binop String SExpr SExpr
	| Unop String SExpr
	deriving (Eq, Show)

sRef s = SRef s
vRef v i = VecRef v i
mRef m i j = MatRef m i j
bOp o s1 s2 = Binop o s1 s2
uOp o s = Unop o s