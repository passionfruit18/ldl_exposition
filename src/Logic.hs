{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Logic where


{-|
PropLogic is a data type for logics based on propositional logic-
particularly modal logics.
-}
data PropLogic p u b =
  PropConst Bool |
  PropVar p |
  Not (PropLogic p u b) |
  And (PropLogic p u b) (PropLogic p u b) |
  Or (PropLogic p u b) (PropLogic p u b) |
  Unary u (PropLogic p u b) | -- unary modal operators u
  Binary b (PropLogic p u b) (PropLogic p u b) deriving (Eq, Ord) -- binary modal operators b

deriving instance (Show p, Show u, Show b) => Show (PropLogic p u b)

foldLog :: (Bool -> r) ->
           (p -> r) ->
           (r -> r) ->
           (r -> r -> r) -> 
           (r -> r -> r) ->
           (u -> r -> r) ->
           (b -> r -> r -> r) ->
           (PropLogic p u b) ->
           r
foldLog cr pr nr ar orr ur br = f where
    f (PropConst c) = cr c
    f (PropVar p) = pr p
    f (Not log) = nr $ f log
    f (And log1 log2) = ar (f log1) (f log2)
    f (Or log1 log2) = orr (f log1) (f log2)
    f (Unary u log) = ur u (f log)
    f (Binary b log1 log2) = br b (f log1) (f log2)

mapLog :: (Bool -> Bool) -> (p -> q) -> PropLogic p u b -> PropLogic q u b
mapLog cr pr = foldLog (PropConst . cr) (PropVar . pr) Not And Or Unary Binary

satisfyBasicProp :: (p -> Bool) -> BasicPropLogic p -> Bool
satisfyBasicProp interpret =
  foldLog id interpret not (&&) (||) (\x y -> True) (\x y z -> True)



connect :: String -> String -> String -> String
connect op s1 s2 = "(" ++ s1 ++ op ++ s2 ++ ")"

logic_pretty_print :: Show p =>
  (u -> String -> String) -> (b -> String -> String -> String) ->
  PropLogic p u b -> String
logic_pretty_print =
	foldLog show show -- propositional constants and variables
	("!"++) -- not
	(connect "&&")
	(connect "||")

logic_pretty_print_ =
  foldLog show id
  ("!"++)
  (connect "&&")
  (connect "||")

data Void --empty type
deriving instance Eq (Void)
deriving instance Ord (Void)
deriving instance Show (Void)
type BasicPropLogic p = PropLogic p Void Void
data UnaryLTL = Next
data BinaryLTL = Until
type LTLogic p = PropLogic p UnaryLTL BinaryLTL

type LDLogic p = PropLogic p (Reg p) Void
data Reg p =
  Base (BasicPropLogic p) |
  Test (LDLogic p) |
  Plus (Reg p) (Reg p) |
  Comp (Reg p) (Reg p) |
  Star (Reg p)

deriving instance Show p => Show (Reg p)
foldReg :: (BasicPropLogic p -> r) ->
			  (LDLogic p -> r) ->
			  (r -> r -> r) ->
			  (r -> r -> r) ->
			  (r -> r) -> Reg p -> r
foldReg br fr prr crr sr reg = f reg where
	f (Base b) = br b
	f (Test f) = fr f
	f (Plus p1 p2) = prr (f p1) (f p2)
	f (Comp p1 p2) = crr (f p1) (f p2)
	f (Star p) = sr (f p)
	

trueReg :: Reg p
trueReg = Base $ PropConst $ True
{-|
"diamond p" is a unary modality that transforms LDL formulae.
e.g. <true> f where f is an LDL formula, is diamond true f.
square is its dual.
-}
diamond :: Reg p -> LDLogic p -> LDLogic p
diamond = Unary
square :: Reg p -> LDLogic p -> LDLogic p
square reg l = Not $ diamond reg $ Not l
test_only :: Reg p -> Bool
test_only = foldReg (\b -> False) (\f -> True) (&&) (&&) id where


ltl2ldl :: LTLogic p -> LDLogic p
ltl2ldl = foldLog PropConst PropVar Not And Or u b where
    u Next ld = diamond trueReg ld
    b Until ld1 ld2 = diamond (Star (Comp (Test ld1) trueReg)) ld2

{-
To translate LDL to AFAs, we will need negation normal-form (nnf) LDL.
This will have square as a built in operator rather than derived from diamond.
-}

type LDLogicNNF p = PropLogic (Literal p) (DiamondOrSquare p) Void
data DiamondOrSquare p = Diamond (Reg p) | Square (Reg p)

negate_dos :: DiamondOrSquare p -> DiamondOrSquare p
negate_dos (Diamond x) = (Square x)
negate_dos (Square x) = (Diamond x)

data Literal p = Positive p | Negative p
negate_literal :: Literal p -> Literal p
negate_literal (Positive x) = Negative x
negate_literal (Negative x) = Positive x

negateNNF :: LDLogicNNF p -> LDLogicNNF p
negateNNF = foldLog cr pr nr arr orr ur br where
  cr c = PropConst (not c)
  pr lit = PropVar (negate_literal lit)
  nr = id -- this is the 'double negatives cancel' part
  arr log1 log2 = Or log1 log2 -- Ors turn into Ands and vice versa
  orr log1 log2 = And log1 log2
  ur dos log1 = Unary (negate_dos dos) log1 -- Square modalities turn into Diamonds / Brackets into Angle Brackets
  br b log1 log2 = error "Should not be any binary modalities in negateNNF"

ldl2nnf :: LDLogic p -> LDLogicNNF p
ldl2nnf = foldLog PropConst PropVar negateNNF And Or ur br where
  ur reg log1 = Unary (True, reg) log1
  br b log1 log2 = error "Should not be any binary modalities"
