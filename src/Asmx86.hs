module Asmx86(
	toAsm) where

import Data.Map as M

-- Some convenience definitions to eliminate "magic numbers"
base = 0
wordBytes = 4
doubleWidth = 8

data ASMState
	= AS {
		namesToOffsets :: Map String Int,
		availIntRegs :: [Register],
		availFloatRegs :: [Register],
		instructions :: [Instruction],
		nextLabel :: Int
	}

type Instruction = String
type Register = String

genInstr :: ASMState -> Instruction -> ASMState
genInstr (AS n i f instrs l) instr = (AS n i f (instrs ++ [instr]) l)

getFloatReg :: ASMState -> Register
getFloatReg (AS _ _ (r:rest) _ _) = r
getFloatReg _ = error $ "No floating point registers available"

getIntReg :: ASMState -> Register
getIntReg (AS _ (ir:rest) _ _ _) = ir
getIntReg _ = error $ "No integer registers available"

getEAX :: ASMState -> Register
getEAX (AS _ intRegs _ _ _) = if Prelude.elem eax intRegs
	then eax
	else error $ "%eax is not available"

-- DataBlock manipulation functions
dataBlock = fromList [("elems", base), ("rows", wordBytes), ("cols", 2*wordBytes)]

fieldOffset :: String -> Int
fieldOffset fieldName = case M.lookup fieldName dataBlock of
	Just off -> off
	Nothing -> error $ fieldName ++ " is not a field of DataBlock"

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
xmm0 = "%xmm0"
xmm1 = "%xmm0"
xmm2 = "%xmm0"
xmm3 = "%xmm0"
xmm4 = "%xmm0"
xmm5 = "%xmm0"
xmm6 = "%xmm0"
xmm7 = "%xmm0"
xmm8 = "%xmm0"
xmm9 = "%xmm0"
xmm10 = "%xmm0"
xmm11 = "%xmm0"
xmm12 = "%xmm0"
xmm13 = "%xmm0"
xmm14 = "%xmm0"
xmm15 = "%xmm0"

eax = "%eax"
ebx = "%ebx"
ecx = "%ecx"
edx = "%edx"
esi = "%esi"
edi = "%edi"

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
instrs iList = concat $ Prelude.map tbln iList

tbln str = "\t" ++ str ++ "\n"

ln str = str ++ "\n"