module LinearMatrixCode(
	MatCodeFunction, LinMatCode, matCodeFunction,
	instructions, genD, defD, copy, add, sub, times,
	sTimes, neg, inv, trans, result, scalarLoopFunction) where

import DataProperties
import ScalarLoopCode

data MatCodeFunction = MCF String [LinMatCode] [LinMatCode] [LinMatCode]
	deriving (Eq)

instance Show MatCodeFunction where
	show = showMatCodeFunc

showMatCodeFunc :: MatCodeFunction -> String
showMatCodeFunc (MCF name args body retVals) =
	showCodeBlock body

showCodeBlock body = concat $ map (\instr -> ('\n':(show instr))) body

matCodeFunction :: String -> [LinMatCode] -> [LinMatCode] -> [LinMatCode] -> MatCodeFunction
matCodeFunction name args body retVals = MCF name args body retVals

data LinMatCode
	= Unop String LinMatCode LinMatCode
	| Binop String LinMatCode LinMatCode LinMatCode
	| GenD String Shape
	| DefD String [Float] Shape
	deriving (Eq)

instance Show LinMatCode where
	show = showLinMatCode

showLinMatCode :: LinMatCode -> String
showLinMatCode (Binop name a1 a2 a3) = showBinInstr name a1 a2 a3
showLinMatCode (Unop name a1 a2) = showUnInstr name a1 a2
showLinMatCode (GenD name _) = name
showLinMatCode (DefD name _ _) = name

result :: LinMatCode -> LinMatCode
result (Binop _ _ _ r) = r
result (Unop _ _ r) = r

showBinInstr name a1 a2 a3 = name ++ " " ++ show a1 ++ " " ++ show a2 ++ " " ++ show a3
showUnInstr name a1 a2 = name ++ " " ++ show a1 ++ " " ++ show a2

copy d1 d2 = Unop "copy" d1 d2
add d1 d2 d3 = Binop "add" d1 d2 d3
sub d1 d2 d3 = Binop "sub" d1 d2 d3
times d1 d2 d3 = Binop "times" d1 d2 d3
sTimes d1 d2 d3 = Binop "stimes" d1 d2 d3
neg d1 d2 = Unop "neg" d1 d2
inv d1 d2 = Unop "inv" d1 d2
trans d1 d2 = Unop "trans" d1 d2

genD name shape = GenD name shape
defD name vals shape = DefD name vals shape

instructions :: MatCodeFunction -> [LinMatCode]
instructions (MCF _ _ code _) = code

-- Code for conversion to scalar loop form
scalarLoopFunction :: MatCodeFunction -> ScalarLoopFunction
scalarLoopFunction (MCF name args body retVals) = scalarLoopCode name argDecs [] retDecs
	where
		argDecs = map toDec args
		retDecs = map toDec retVals

toDec :: LinMatCode -> Declaration
toDec (GenD name shape) = case dimensionStrings shape of
	("1", "1") -> sDec name
	("1", d) -> rDec name d
	(d, "1") -> cDec name d
	(r, c) -> gmDec name r c
toDec (DefD name _ shape) = case dimensionStrings shape of
	("1", "1") -> sDec name
	("1", d) -> rDec name d
	(d, "1") -> cDec name d
	(r, c) -> gmDec name r c