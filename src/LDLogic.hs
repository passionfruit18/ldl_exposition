{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : LDLogic
Description : Linear Dynamic Logic

-}
module LDLogic where
import Logic
import Reg
import Prelude hiding (negate)
-- We'll define our own negate.
import Control.Newtype

-- * Ordinary LDLogic (Linear Dynamic Logic)

-- ** Reg type for LDLogic

newtype Reg p = Reg (Reg_T p (LDLogic p))
instance Newtype (Reg p) (Reg_T p (LDLogic p)) where
  pack = Reg
  unpack (Reg x) = x
--deriving instance Show p => Show (Reg p)
diamond :: Reg p -> LDLogic p -> LDLogic p
diamond = Unary
square :: Reg p -> LDLogic p -> LDLogic p
square reg l = Not $ diamond reg $ Not l

-- ** LDLogic itself

type LDLogic p = PropLogic p (Reg p) Void

ltl2ldl :: LTLogic p -> LDLogic p
ltl2ldl = foldLog PropConst PropVar Not And Or u b where
    u Next ld = diamond (Reg trueReg) ld
    b Until ld1 ld2 = diamond (Reg (Star (Comp (Test ld1) trueReg))) ld2

ldl_propositions :: LDLogic p -> [p]
ldl_propositions = propositions urr (error "binary modality") where
  urr reg ps = reg_propositions reg ++ ps

reg_propositions :: Reg p -> [p]
reg_propositions = (foldReg basic_propositions ldl_propositions (++) (++) id) . op Reg

basic_propositions :: BasicPropLogic p -> [p]
basic_propositions = propositions (error "unary modality") (error "binary modality")

-- * LDLogicNNF (LDL in Negation Normal Form)

-- ** Reg type for LDLogicNNF
newtype RegNNF p = RegNNF (Reg_T p (LDLogicNNF p)) deriving (Eq, Ord, Show)
instance Newtype (RegNNF p) (Reg_T p (LDLogicNNF p)) where
  pack = RegNNF
  unpack (RegNNF x) = x

-- ** LDLogicNNF itself and accompanying helper functions


-- |Self explanatory, I hope...
class Negatable a where
  negate :: a -> a
  
{-|
To translate LDL to AFAs, we will need negation normal-form (nnf) LDL.
This will have literals rather than propositions and square as a built in operator rather than derived from diamond (DOS).
-}
type LDLogicNNF p = PropLogic (Literal p) (DOS p) Void
instance Negatable (LDLogicNNF p) where
  negate = foldLog cr pr nr arr orr ur br where
    cr c = PropConst (not c)
    pr lit = PropVar (negate lit)
    nr = id -- this is the 'double negatives cancel' part
    arr log1 log2 = Or log1 log2 -- Ors turn into Ands and vice versa
    orr log1 log2 = And log1 log2
    ur dos log1 = Unary (negate dos) log1 -- Square modalities turn into Diamonds and vice versa
    br b log1 log2 = error "Should not be any binary modalities in negateNNF"

-- |Literal of p
data Literal p = Positive p | Negative p deriving (Eq, Ord, Show)
instance Negatable (Literal p) where
  negate (Positive x) = (Negative x)
  negate (Negative x) = (Positive x)

-- |DOS: Diamond or Square modality.
data DOS p = Diamond (RegNNF p) | Square (RegNNF p) deriving (Eq, Ord, Show)
instance Negatable (DOS p) where
  negate (Diamond x) = (Square x)
  negate (Square x) = (Diamond x)

-- ** Translation of LDL to NNF (including accompanying Reg to NNF translation)

ldl2nnf :: LDLogic p -> LDLogicNNF p
ldl2nnf = foldLog PropConst (PropVar . Positive) negate And Or ur br where
  ur reg log1 = Unary (Diamond (reg2nnf reg)) log1
  br b log1 log2 = error "Should not be any binary modalities"

reg2nnf :: Reg p -> RegNNF p
{-
reg2nnf (Reg x) = RegNNF $ (foldReg Base fr Plus Comp Star) x where -- Reg and RegNNF wrapping and unwrapping the newtypes
  fr = Test . ldl2nnf
-}
reg2nnf = over Reg (foldReg Base fr Plus Comp Star) where
  fr = Test . ldl2nnf
-- do we have to nnf the BasicPropLogic? Not sure. Well I don't think so.
{-
-- ** Eq and Ord derivation

deriving instance Ord p => Ord (LDLogicNNF p)
deriving instance Ord p => Ord (RegNNF p)
deriving instance Ord p => Ord (Literal p)
deriving instance Ord p => Ord (DOS p)

deriving instance Eq p => Eq (LDLogicNNF p)
deriving instance Eq p => Eq (RegNNF p)
deriving instance Eq p => Eq (Literal p)
deriving instance Eq p => Eq (DOS p)
-}










