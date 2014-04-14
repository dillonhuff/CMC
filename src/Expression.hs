module Expression(
	Expression,
	assign, operator,
	unaryOp, binaryOp,
	float, matrix,
	identifier) where

assign :: Expression -> Expression -> Expression
assign e1 e2 = Assign e1 e2

identifier :: String -> Expression
identifier name = Identifier name

operator name = Op name

unaryOp :: Expression -> Expression -> Expression
unaryOp op operand = UnaryOp op operand

binaryOp :: Expression -> Expression -> Expression -> Expression
binaryOp op operand1 operand2 =
	BinaryOp op operand1 operand2

matrix :: Int -> Int -> [Float] -> Expression
matrix rows cols vals = Matrix rows cols vals

-- Scalars are represented as 1 by 1 matrices
float :: Float -> Expression
float val = Matrix 1 1 [val]

data Expression
	= Identifier String
	| Assign Expression Expression
	| Matrix Int Int [Float]
	| UnaryOp Expression Expression
	| BinaryOp Expression Expression Expression
	| Op String
	deriving (Eq, Show)