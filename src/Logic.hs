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

propositions :: (u -> [p] -> [p]) -> (b -> [p] -> [p] -> [p]) -> PropLogic p u b -> [p]
propositions = foldLog (\c -> []) (\p -> [p]) id (++) (++) 


connect :: String -> String -> String -> String -> String
connect op sep s1 s2 = "(" ++ s1 ++ sep ++ op ++ sep ++ s2 ++ ")"

logic_pretty_print :: Show p =>
  (u -> String -> String) -> (b -> String -> String -> String) ->
  PropLogic p u b -> String
logic_pretty_print =
	foldLog show show -- propositional constants and variables
	("!"++) -- not
	(connect "&&" " ")
	(connect "||" " ")

type BasicPropLogic p = PropLogic p Void Void
basic_pretty_print :: Show p => BasicPropLogic p -> String
basic_pretty_print = logic_pretty_print
  (\u s -> "")
  (\b s1 s2 -> "")



basic_pretty_print_with :: (p -> String) -> BasicPropLogic p -> String
basic_pretty_print_with printer = 
  foldLog show printer
  ("!"++) -- not
  (connect "&&" " ")
  (connect "||" " ")
  (\u s -> "")
  (\b s1 s2 -> "")

data Void --empty type
deriving instance Eq (Void)
deriving instance Ord (Void)
deriving instance Show (Void)
data UnaryLTL = Next
data BinaryLTL = Until
type LTLogic p = PropLogic p UnaryLTL BinaryLTL