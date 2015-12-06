module Samples where
import Automata
import Data.Set.Monad as S
import Data.Map as M
{- 
type Alphabet = Char

as :: S.Set Alphabet
as = S.fromList ['a','b','c']

type State = Int
qs :: S.Set State
qs = S.fromList [1,2]

afa1 = FA as qs 1 t S.empty where
	t = M.fromList [((1,'a'), PropConst false)]
	-- 1 x a -> false

-}