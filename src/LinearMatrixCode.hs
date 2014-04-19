module LinearMatrixCode(
	MatCodeFunction, LinMatCode, matCodeFunction,
	instructions, genD, defD, copy, add, sub, times,
	sTimes, neg, inv, trans, result) where

import DataProperties

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
	deriving (Eq)

instance Show LinMatCode where
	show = showLinMatCode

showLinMatCode :: LinMatCode -> String
showLinMatCode (Add a1 a2 a3) = showBinInstr "add" a1 a2 a3
showLinMatCode (Sub a1 a2 a3) = showBinInstr "sub" a1 a2 a3
showLinMatCode (Times a1 a2 a3) = showBinInstr "times" a1 a2 a3
showLinMatCode (STimes a1 a2 a3) = showBinInstr "stimes" a1 a2 a3
showLinMatCode (Copy a1 a2) = showUnInstr "copy" a1 a2
showLinMatCode (Neg a1 a2) = showUnInstr "neg" a1 a2
showLinMatCode (Inv a1 a2) = showUnInstr "inv" a1 a2
showLinMatCode (Trans a1 a2) = showUnInstr "trans" a1 a2
showLinMatCode (GenD name _) = name
showLinMatCode (DefD name _ _) = name

result :: LinMatCode -> LinMatCode
result (Add _ _ r) = r
result (Sub _ _ r) = r
result (Times _ _ r) = r
result (STimes _ _ r) = r
result (Copy _ r) = r
result (Neg _ r) = r
result (Inv _ r) = r
result (Trans _ r) = r

showBinInstr name a1 a2 a3 = name ++ " " ++ show a1 ++ " " ++ show a2 ++ " " ++ show a3
showUnInstr name a1 a2 = name ++ " " ++ show a1 ++ " " ++ show a2

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