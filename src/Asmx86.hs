module Asmx86(
	toAsm) where

toAsm :: String -> String
toAsm name = (prologue name) ++ body ++ epilogue

prologue funcName = (fileDec funcName) ++ textSeg ++ (globl undFunc) ++ (def undFunc) ++ (label undFunc)
	where
		undFunc = "_" ++ funcName

body = bodyStart ++ bodyEnd

bodyStart = (pushl "%ebp") ++ (movl "%esp" "%ebp")

bodyEnd = leave ++ ret

epilogue = (def "_malloc") ++ (def "_free")


-- Helper functions for instruction generation
label name = ln (name ++ ":")

pushl reg = tbln ("pushl " ++ reg)

movl dest src = tbln ("movl " ++ dest ++ ", " ++ src)

leave = tbln "leave"

ret = tbln "ret"

-- Helper functions for file declarations
fileDec name = tbln (".file\t" ++ "\"" ++ name ++ ".cmc\"")

textSeg = tbln ".text"

globl name = ln (".globl " ++ name)

def name = tbln (".def\t" ++ name ++ ";\t.scl\t2;\t.type 32; .endef")

-- General string format helpers
tbln str = "\t" ++ str ++ "\n"

ln str = str ++ "\n"

