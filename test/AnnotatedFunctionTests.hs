module AnnotatedFunctionTests(
	annotatedFunctionTests) where

import AnnotatedFunction
import DataProperties
import LinearMatrixCode
import TestUtils

annotatedFunctionTests = testFunction funcInstructions testFuncs

funcInstructions = instructions . linearMatrixCode

testFuncs =
	[noArgFuncAssign
	,noArgFuncAdd
	,noArgFuncSub
	,noArgFuncTimes
	,noArgFuncScalarTimes
	,noArgFuncNeg
	,noArgFuncInv
	,noArgFuncTrans]

noArgFuncAssign =
	(annotatedFunction "no" [] [aBinop "=" (aId "K" scalar) (aMat [1] scalar) scalar] []
	,[copy (defD "res0" [1] scalar) (genD "K" scalar)])

noArgFuncAdd =
	(annotatedFunction "no" [] [aBinop "+" (aId "K" scalar) (aMat [1] scalar) scalar] []
	,[add (genD "K" scalar) (defD "res0" [1] scalar) (genD "res1" scalar)])

noArgFuncSub =
	(annotatedFunction "no" [] [aBinop "-" (aId "K" scalar) (aMat [1] scalar) scalar] []
	,[sub (genD "K" scalar) (defD "res0" [1] scalar) (genD "res1" scalar)])

noArgFuncTimes =
	(annotatedFunction
		"no"
		[]
		[aBinop "*" (aId "K" (genGeneral "a" "b")) (aId "X" (genGeneral "b" "c")) (genGeneral "a" "c")]
		[]
	,[times (genD "K" (genGeneral "a" "b")) (genD "X" (genGeneral "b" "c")) (genD "res0" (genGeneral "a" "c"))])

noArgFuncScalarTimes =
	(annotatedFunction
		"no"
		[]
		[aBinop ".*" (aId "K" scalar) (aId "X" (genGeneral "b" "c")) (genGeneral "b" "c")]
		[]
	,[sTimes (genD "K" scalar) (genD "X" (genGeneral "b" "c")) (genD "res0" (genGeneral "b" "c"))])

noArgFuncNeg =
	(annotatedFunction
		"oh"
		[]
		[aUnop "-" (aId "S" (genSymmetric "I")) (genSymmetric "I")]
		[]
	,[neg (genD "S" (genSymmetric "I")) (genD "res0" (genSymmetric "I"))])

noArgFuncInv =
	(annotatedFunction
		"oh"
		[]
		[aUnop "!" (aId "S" (genSymmetric "I")) (genSymmetric "I")]
		[]
	,[inv (genD "S" (genSymmetric "I")) (genD "res0" (genSymmetric "I"))])

noArgFuncTrans =
	(annotatedFunction
		"oh"
		[]
		[aUnop "'" (aId "S" (genSymmetric "I")) (genSymmetric "I")]
		[]
	,[trans (genD "S" (genSymmetric "I")) (genD "res0" (genSymmetric "I"))])