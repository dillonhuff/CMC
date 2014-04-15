module Lexer(
	PosTok, Tok, lexer, tok, pos,
	opTok, idTok, floatTok, tokName,
	floatTokValue,
	isIdentifierTok, isOperatorTok, isFloatTok,
	isFuncIdTok,
	floatPosTok, identifierPosTok,
	opPosTok, funcIdPosTok,
	lparenPosTok, rparenPosTok,
	lbracketPosTok, rbracketPosTok,
	commaPosTok, equalsPosTok,
	semicolonPosTok, funcPosTok, returnPosTok,
	lparenTok, rparenTok,
	lbracketTok, rbracketTok,
	commaTok, equalsTok, funcIdTok,
	semicolonTok, funcTok, returnTok) where

import ErrorHandling
import Text.Parsec.Pos
import Text.ParserCombinators.Parsec

lexer :: String -> Error [PosTok]
lexer str = case parse pToks "Lexer" str of
	Left err -> Failed $ show err
	Right toks -> Succeeded toks

data PosTok = PT Tok SourcePos
	
instance Show PosTok where
	show = showPT

instance Eq PosTok where
	(==) = ptEq

showPT :: PosTok -> String
showPT (PT t _) = show t

ptEq (PT t1 _) (PT t2 _) = t1 == t2

tok :: PosTok -> Tok
tok (PT t _) = t

pos :: PosTok -> SourcePos
pos (PT _ p) = p

isOperatorTok (Op _) = True
isOperatorTok _ = False

isFloatTok (FloatTok _) = True
isFloatTok _ = False

isIdentifierTok (Var _) = True
isIdentifierTok _ = False

isFuncIdTok (FuncId _) = True
isFuncIdTok _ = False

tokName (PT (Var name) _) = name
tokName (PT (FuncId name) _) = name
tokName (PT (Op name) _) = name

floatTokValue (PT (FloatTok val) _) = val

dummyPosTok :: Tok -> PosTok
dummyPosTok t = PT t (newPos "DUMMY" 0 0)

identifierPosTok name = dummyPosTok (Var name)
funcIdPosTok name = dummyPosTok (FuncId name)
opPosTok name = dummyPosTok (Op name)
floatPosTok value = dummyPosTok (FloatTok value)
lparenPosTok = dummyPosTok LPAREN
rparenPosTok = dummyPosTok RPAREN
lbracketPosTok = dummyPosTok LBRACET
rbracketPosTok = dummyPosTok RBRACKET
commaPosTok = dummyPosTok COMMA
equalsPosTok = dummyPosTok EQUALS
semicolonPosTok = dummyPosTok SEMICOLON
funcPosTok = dummyPosTok FUNC
returnPosTok = dummyPosTok RETURN

idTok name = (Var name)
funcIdTok name = (FuncId name)
opTok name = (Op name)
floatTok value = (FloatTok value)
lparenTok = LPAREN
rparenTok = RPAREN
lbracketTok = LBRACET
rbracketTok = RBRACKET
commaTok = COMMA
equalsTok = EQUALS
semicolonTok = SEMICOLON
funcTok = FUNC
returnTok = RETURN

data Tok
	= Op String
	| Var String
	| FuncId String
	| FloatTok Float
	| LPAREN
	| RPAREN
	| LBRACET
	| RBRACKET
	| SEMICOLON
	| COMMA
	| EQUALS
	| FUNC
	| RETURN
	deriving (Eq, Show)

reservedTokens =
	[("(", LPAREN), (")", RPAREN)
	,("[", LBRACET), ("]", RBRACKET)
	,("=", EQUALS), (",", COMMA)
	,(";", SEMICOLON), ("return", RETURN)
	,("func", FUNC)]

pToks = endBy pTok spaces

pTok = do
	pos <- getPosition
	tok <- pOpTok
		<|> try pReservedTok
		<|> try pFuncIdTok
		<|> pVarTok
		<|> pFloatTok
	return $ PT tok pos

pOpTok = do
	operator <- string "+"
		<|> string "-"
		<|> string "*"
		<|> string ".*"
		<|> string "!"
		<|> string "'"
	return $ Op operator

pReservedTok = do
	reserved <- string "("
		<|> string ")"
		<|> string "["
		<|> string "]"
		<|> string "="
		<|> string ","
		<|> string ";"
		<|> string "return"
		<|> string "func"
	return $ case lookup reserved reservedTokens of
		Just t -> t
		Nothing -> error $ "No such token as " ++ reserved

pFuncIdTok = do
	firstChar <- lower
	rest <- many alphaNum
	return $ FuncId (firstChar:rest)

pVarTok = do
	startChar <- upper
	rest <- many alphaNum
	return $ Var (startChar:rest)

pFloatTok = try pFloatDecimal <|> pFloatNoDecimal

pFloatNoDecimal = do
	leading <- many1 digit
	power <- option "" pExponent
	return $ FloatTok (read (leading ++ power))

pFloatDecimal = do
	leading <- many1 digit
	dot <- char '.'
	trailing <- many1 digit
	power <- option "" pExponent
	return $ FloatTok (read (leading ++ [dot] ++ trailing ++ power))

pExponent = do
	e <- oneOf "eE"
	sign <- option "" (string "-")
	pow <- many1 digit
	return (e:sign ++ pow)