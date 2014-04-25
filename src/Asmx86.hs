module Asmx86(
	toAsm) where

toAsm :: String -> String
toAsm name = (prologue name) ++ epilogue

prologue funcName = (fileDec funcName) ++ textSeg ++ (globl undFunc) ++ (def undFunc) ++ (label undFunc)
	where
		undFunc = "_" ++ funcName

epilogue = (def "_malloc") ++ (def "_free")

fileDec name = tbln (".file\t" ++ "\"" ++ name ++ ".cmc\"")

textSeg = tbln ".text"

globl name = ln (".globl " ++ name)

def name = tbln (".def\t" ++ name ++ ";\t.scl\t2;\t.type 32; .endef")

label name = ln (name ++ ":")

tbln str = "\t" ++ str ++ "\n"

ln str = str ++ "\n"

