module Lexer(
	lexer,
	opTok, varTok, floatTok) where

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

dummyPosTok :: Tok -> PosTok
dummyPosTok t = PT t (newPos "DUMMY" 0 0)

varTok name = dummyPosTok (Var name)
opTok name = dummyPosTok (Op name)
floatTok value = dummyPosTok (FloatTok value)

data Tok
	= Op String
	| Var String
	| FloatTok Float
	deriving (Eq, Show)

pToks = endBy pTok spaces

pTok = do
	pos <- getPosition
	tok <- pOpTok
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