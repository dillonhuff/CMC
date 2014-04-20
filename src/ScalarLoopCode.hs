module ScalarLoopCode(
	ScalarLoopFunction, Iteration,
	Declaration, iterateOver, toRef,
	scalarLoopCode, assign, swap,
	sDec, cDec, rDec, gmDec,
	uOp, bOp) where

import DataProperties

scalarLoopCode :: String -> [Declaration] -> [Iteration] -> [Declaration] -> ScalarLoopFunction
scalarLoopCode name args body retVals = SCF name args body retVals

data ScalarLoopFunction = SCF String [Declaration] [Iteration] [Declaration]
	deriving (Eq)

instance Show ScalarLoopFunction where
	show = showSLF

showSLF (SCF name argDecs body retVals) = defines ++ funcDec ++ bodyStr ++ retSt ++ "\n}"
	where
		defines = cTypeDef dataBlockStruct dataBlockNames ++ "\n"
		funcDec = "void " ++ name ++ "(" ++ (argList (argDecs ++ retVals)) ++ ") {"
		bodyStr = indent $  intDecs ++ (concat $ map (((++) "\n") . show) body)
		retSt = indent $ "\nreturn;"

argList [] = []
argList (arg:[]) = show arg
argList (arg:rest) = show arg ++ ", " ++ (argList rest)

intDecs = "\n" ++ (intDec "i") ++ (intDec "j") ++ (intDec "k")
intDec var = cst $ "int " ++ var

data Iteration
	= Iteration {
		referenced :: [String],
		updated :: [String],
		starts :: [String],
		ends :: [String],
		body :: [Update]}
	deriving (Eq)

iteration refs ups st en b = Iteration {
	referenced = refs,
	updated = ups,
	starts = st,
	ends = en,
	body = b
	}

instance Show Iteration where
	show = showIteration

showIteration (Iteration _ _ st en bod) = showLoops ['i'..'z'] st en bod

showLoops :: [Char] -> [String] -> [String] -> [Update] -> String
showLoops (v:restVars) (start:restSts) (end:restEnds) updates = dec ++ loopBody ++ "\n}"
	where
		dec = loopDec (v:"") start end
		loopBody = indent ("\n" ++ showLoops restVars restSts restEnds updates)
showLoops _ [] [] updates = concat $ map show updates

loopDec :: String -> String -> String -> String
loopDec var start end = forDec var start end

forDec var start end = "for (" ++ startCond ++ endCond ++ updateRule ++ ") {"
	where
		startCond = var ++ " = " ++ start ++ "; "
		endCond = var ++ " < " ++ end ++ "; "
		updateRule = var ++ "++"

iterateOver :: Declaration -> Update -> Iteration
iterateOver (Scalar n) u =
	Iteration {
		referenced = refd u,
		updated = upd u,
		starts = [],
		ends = [],
		body = [u]
	}
iterateOver (RowVector n d) u=
	Iteration {
		referenced = refd u,
		updated = upd u,
		starts = ["0"],
		ends = [d],
		body = [u]
	}
iterateOver (ColVector n d) u=
	Iteration {
		referenced = refd u,
		updated = upd u,
		starts = ["0"],
		ends = [d],
		body = [u]
	}
iterateOver (GeneralMatrix n r1 c1) u =
	Iteration {
		referenced = refd u,
		updated = upd u,
		starts = ["0", "0"],
		ends = [r1, c1],
		body = [u]
	}

data Update
	= Assign SExpr SExpr
	| Swap SExpr SExpr
	deriving (Eq)

instance Show Update where
	show = showUpdate

showUpdate :: Update -> String
showUpdate (Assign s1 s2) = cst (show s1 ++ " = " ++ show s2)
showUpdate (Swap s1 s2) =  concat $ map show [saveTmp, assignS1S2, assignS2Tmp]
	where
		saveTmp = Assign (sRef "tmp") s1
		assignS1S2 = Assign s1 s2
		assignS2Tmp = Assign s2 (sRef "tmp")

assign s1 s2 = Assign s1 s2
swap s1 s2 = Swap s1 s2

upd :: Update -> [String]
upd (Assign s1 _) = refNames s1
upd (Swap s1 s2) = refNames s1 ++ refNames s2

refd :: Update -> [String]
refd (Assign s1 s2) = (refNames s1) ++ (refNames s2)
refd (Swap s1 s2) = (refNames s1) ++ (refNames s2)

-- TODO: Add more specific declarations later. For
-- now all matrices will declare as GeneralMatrix
-- regardless of special properties
data Declaration
	= Scalar String
	| RowVector String String
	| ColVector String String
	| GeneralMatrix String String String
	deriving (Eq)

instance Show Declaration where
	show = showDeclaration

showDeclaration (Scalar s) = "float " ++ s
showDeclaration (RowVector s size) = "ROW_VECTOR " ++ s
showDeclaration (ColVector s size) = "COL_VECTOR " ++ s
showDeclaration (GeneralMatrix s r c) = "MATRIX " ++ s

sDec s = Scalar s
rDec rv d = RowVector rv d
cDec cv d = ColVector cv d
gmDec m r c = GeneralMatrix m r c

toRef (Scalar s) = sRef s
toRef (RowVector s d) = VecRef s "i"
toRef (ColVector s d) = VecRef s "i"
toRef (GeneralMatrix s r c) = MatRef s "i" "j"

data SExpr
	= SRef String
	| VecRef String String
	| MatRef String String String
	| Binop String SExpr SExpr
	| Unop String SExpr
	deriving (Eq)

instance Show SExpr where
	show = showSExpr

showSExpr :: SExpr -> String
showSExpr (SRef n) = n
showSExpr (VecRef n i) = n ++ "->elems[" ++ i ++ "]"
showSExpr (MatRef n i j) = n ++ "->elems[" ++ n ++ "->row*" ++ i ++ "" ++ "+" ++ j ++ "]"
showSExpr (Binop n s1 s2) = show s1 ++ n ++ show s2
showSExpr (Unop n s) = n ++ show s

refNames :: SExpr -> [String]
refNames (SRef name) = [name]
refNames (VecRef name _) = [name]
refNames (MatRef name _ _) = [name]
refNames (Binop _ s1 s2) = (refNames s1) ++ (refNames s2)
refNames (Unop _ s) = refNames s

sRef s = SRef s
vRef v i = VecRef v i
mRef m i j = MatRef m i j
bOp o s1 s2 = Binop o s1 s2
uOp o s = Unop o s

-- Helper functions for translation to C code
csts :: [String] -> String
csts statements = concat $ map cst statements

cst :: String -> String
cst s = s ++ ";\n"

indent :: String -> String
indent [] = []
indent ('\n':rest) = ('\n':'\t':(indent rest))
indent (c:rest) = (c:(indent rest))

cTypeDef t s = cst $ "typedef " ++ t ++ " " ++ s

dataBlockStruct = "struct DataBlock {\n\tfloat *elems;\n\tint row;\n\tint col;\n}"
dataBlockNames = " *MATRIX, *COL_VECTOR, *ROW_VECTOR"