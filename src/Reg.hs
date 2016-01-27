{-|
Module      : Reg
Description : Regular Expression data type for use in LDLogic

Regular expressions and LDLogic are mutually recursive datatypes.
Here I define a template data type Reg_T, which will be instantiated in LDLogic
to define LDLogic and variations thereof (e.g. LDLogicNNF).
-}
module Reg where
import Logic


-- |We define Reg and associated functions before defining LDLogic.
data Reg_T p q =
  Base (BasicPropLogic p) |
  Test q | --Test (LDLogic p) |
  Plus (Reg_T p q) (Reg_T p q) |
  Comp (Reg_T p q) (Reg_T p q) |
  Star (Reg_T p q) deriving (Eq, Ord, Show)

foldReg :: (BasicPropLogic p -> r) ->
			  (q -> r) ->
			  (r -> r -> r) ->
			  (r -> r -> r) ->
			  (r -> r) -> (Reg_T p q) -> r
foldReg br fr prr crr sr reg = f reg where
	f (Base b) = br b
	f (Test f) = fr f
	f (Plus p1 p2) = prr (f p1) (f p2)
	f (Comp p1 p2) = crr (f p1) (f p2)
	f (Star p) = sr (f p)


trueReg :: Reg_T p q
trueReg = Base $ PropConst $ True
{-|
"diamond p" is a unary modality that transforms LDL formulae.
e.g. <true> f where f is an LDL formula, is diamond true f.
square is its dual.
-}

test_only :: Reg_T p q -> Bool
test_only = foldReg (\b -> False) (\f -> True) (&&) (&&) id

