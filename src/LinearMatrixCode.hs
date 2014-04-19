module LinearMatrixCode(
	MatCodeFunction, LinMatCode, matCodeFunction,
	instructions, genD, defD, copy, add, sub, times,
	sTimes, neg, inv, trans) where

import DataProperties

data MatCodeFunction = MCF String [LinMatCode] [LinMatCode] [LinMatCode]
	deriving (Eq, Show)

matCodeFunction :: String -> [LinMatCode] -> [LinMatCode] -> [LinMatCode] -> MatCodeFunction
matCodeFunction name args body retVals = MCF name args body retVals

data LinMatCode
	= Copy LinMatCode LinMatCode
	| Add LinMatCode LinMatCode LinMatCode
	| Sub LinMatCode LinMatCode LinMatCode
	| Times LinMatCode LinMatCode LinMatCode
	| STimes LinMatCode LinMatCode LinMatCode
	| Neg LinMatCode LinMatCode
	| Inv LinMatCode LinMatCode
	| Trans LinMatCode LinMatCode
	| GenD String Shape
	| DefD String [Float] Shape
	deriving (Eq, Show)

copy d1 d2 = Copy d1 d2
add d1 d2 d3 = Add d1 d2 d3
sub d1 d2 d3 = Sub d1 d2 d3
times d1 d2 d3 = Times d1 d2 d3
sTimes d1 d2 d3 = STimes d1 d2 d3
neg d1 d2 = Neg d1 d2
inv d1 d2 = Neg d1 d2
trans d1 d2 = Trans d1 d2

genD name shape = GenD name shape
defD name vals shape = DefD name vals shape

instructions :: MatCodeFunction -> [LinMatCode]
instructions (MCF _ _ code _) = code