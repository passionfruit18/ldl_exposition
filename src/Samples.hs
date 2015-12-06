module Samples where
import Automata
import Logic
import Data.Set.Monad as S
import Data.Map as M


newtype Label = L Char deriving (Eq, Ord)

instance Show Label where
	show (L c) = [c]

type Alphabet = Label

as :: S.Set Alphabet
as = S.fromList $ Prelude.map L ['a','b','c']

type State = Int
qs :: S.Set State
qs = S.fromList [1,2,3,4,5]

afa1 :: AFA State Alphabet
afa1 = FA as qs 1 (ATransition t) S.empty where
	t = M.fromList [((1,L 'a'), PropConst False)]
	-- 1 x a -> false

dfa1 :: DFA State Alphabet
dfa1 = FA as qs 1 (DTransition t) (S.fromList [2,3]) where
	t = M.fromList [((1,L 'a'), 2),
					((2,L 'b'), 1),
					((3,L 'c'), 1),
					((1,L 'b'), 1),
					((1,L 'a'), 3)]

