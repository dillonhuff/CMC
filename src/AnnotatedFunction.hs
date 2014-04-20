module AnnotatedFunction(
	AnnotatedFunction, AExpr, aId, aBinop, aUnop, aMat,
	shapeOf, getIdShape, nameOf,
	annotatedFunction, linearMatrixCode) where

import DataProperties
import LinearMatrixCode

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

-- Code for converting the annotated function into linear matrix code
linearMatrixCode :: AnnotatedFunction -> MatCodeFunction
linearMatrixCode (AF name args body returnVals) =
	matCodeFunction name argData instrs returnData
	where
		argData = map (res . (toLinCode 0)) args
		returnData = map (res . (toLinCode 0)) returnVals
		instrs = aExprsToLinMatCodeInstructions body

aExprsToLinMatCodeInstructions :: [AExpr] -> [LinMatCode]
aExprsToLinMatCodeInstructions exprs =
	exprsToCode 0 exprs

exprsToCode :: Int -> [AExpr] -> [LinMatCode]
exprsToCode _ [] = []
exprsToCode n (e:rest) = (code $ eCode) ++ (exprsToCode (resNum eCode) rest)
	where
		eCode = toLinCode n e

data CodeGenState = CGS { res :: LinMatCode, resNum :: Int, code :: [LinMatCode]}

toLinCode :: Int -> AExpr -> CodeGenState
toLinCode n (ABinop opName arg1 arg2 shape) =
	CGS { res = (result $ last opCode), resNum = finalNum, code = finalCode}
	where
		a1CGS = toLinCode n arg1
		a2CGS = toLinCode (resNum a1CGS) arg2
		finalNum = (resNum a2CGS) + 1
		opCode = binopCode opName shape a1CGS a2CGS
		finalCode = (code a1CGS) ++ (code a2CGS) ++ opCode
toLinCode n (AUnop opName arg shape) =
	CGS { res = (result $ last opCode), resNum = (resNum aCGS) + 1, code = finalCode}
	where
		aCGS = toLinCode n arg
		opCode = unopCode opName shape aCGS
		finalCode = (code aCGS) ++ opCode
toLinCode n (AIdent name shape) = CGS {res = ident, resNum = n, code = []}
	where
		ident = genD name shape
toLinCode n (AMat vals shape) = CGS {res = mat, resNum = n + 1, code = []}
	where
		mat = defD (rName n) vals shape

binopCode :: String -> Shape -> CodeGenState -> CodeGenState -> [LinMatCode]
binopCode opName shape a1CGS a2CGS = case opName of
	"=" -> [copy (res a2CGS) (res a1CGS)]
	"+" -> [add (res a1CGS) (res a2CGS) (genD (rName (resNum a2CGS)) shape)]
	"-" -> [sub (res a1CGS) (res a2CGS) (genD (rName (resNum a2CGS)) shape)]
	"*" -> [times (res a1CGS) (res a2CGS) (genD (rName (resNum a2CGS)) shape)]
	".*" -> [sTimes (res a1CGS) (res a2CGS) (genD (rName (resNum a2CGS)) shape)]
	_ -> error $ opName ++ " is not a binary operator"

unopCode :: String -> Shape -> CodeGenState -> [LinMatCode]
unopCode opName shape aCGS = case opName of
	"-" -> [neg (res aCGS) (genD (rName (resNum aCGS)) shape)]
	"'" -> [trans (res aCGS) (genD (rName (resNum aCGS)) shape)]
	"!" -> [inv (res aCGS) (genD (rName (resNum aCGS)) shape)]
	_ -> error $ opName ++ " is not a unary operator"

rName n = "res" ++ show n