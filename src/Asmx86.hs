module Asmx86(
	toAsm) where

toAsm :: String -> String
toAsm name = (prologue name) ++ body ++ epilogue

prologue funcName = (fileDec funcName) ++ textSeg ++ (globl undFunc) ++ (def undFunc) ++ (label undFunc)
	where
		undFunc = "_" ++ funcName

body = bodyStart ++ bodyEnd

bodyStart = instrs [pushl ebp, movl esp ebp]

bodyEnd = leave ++ ret

epilogue = (def "_malloc") ++ (def "_free")

-- Register names
ebp = "%ebp"
esp = "%esp"

-- Helper functions for instruction generation
label name = ln (name ++ ":")

pushl reg = uIstr "pushl" reg

movl dest src = bInstr "movl" dest src

leave = tbln "leave"

ret = tbln "ret"

uIstr name arg = name ++ " " ++ arg

bInstr name arg1 arg2 = name ++ " " ++ arg1 ++ ", " ++ arg2

-- Helper functions for file declarations
fileDec name = tbln (".file\t" ++ "\"" ++ name ++ ".cmc\"")

textSeg = tbln ".text"

globl name = ln (".globl " ++ name)

def name = tbln (".def\t" ++ name ++ ";\t.scl\t2;\t.type 32; .endef")

-- General string format helpers
instrs iList = concat $ map tbln iList

tbln str = "\t" ++ str ++ "\n"

ln str = str ++ "\n"

