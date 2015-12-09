{-# LANGUAGE MonadComprehensions #-}
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

afa2 :: AFA State Alphabet
afa2 = FA as qs 1 (ATransition t) (S.singleton 3) where
	t = M.fromList [((1, L 'a'), form)]
	form = And (Or (PropVar 3) (PropVar 2)) (Or (PropVar 4) (PropVar 5))

dfa1 :: DFA State Alphabet
dfa1 = FA as qs 1 (DTransition t) (S.fromList [2,3]) where
	t = M.fromList [((1,L 'a'), 2),
					((2,L 'b'), 1),
					((3,L 'c'), 1),
					((1,L 'b'), 1),
					((1,L 'a'), 3)]

dfa2 = reach $ n2d $ a2n afa2 -- This doesn't work! Probably because the translation is a bit too naive.


-- | The reachable part of the subset construction on the reversal of a DFA.
reach :: (Ord a, Ord q) => DFA q a -> DFA q a
reach (FA as qs1 q1 (DTransition dt1) fs1) = 
    (FA as qs2 q2 (DTransition dt2) fs2) where
        q2 = q1
        (qs2, dt2) = process (S.singleton q2, reach (S.singleton q2))
        process (currentReached, currentTransition) =
            -- invariant: currentTransition contains
            -- all transitions from states in currentReached.
            let nextReached = S.fromList (M.elems currentTransition) in
            if currentReached == nextReached
                then (currentReached, currentTransition)
                else process (nextReached,
                            M.union currentTransition
                                    (reach (S.difference nextReached currentReached)))
        reach qs = M.fromList $ S.toList [((q, a), dt1 M.! (q,a)) | q <- qs, a <- as ]
        -- pre: states which transition into s via t on a.
        fs2 = S.intersection fs1 qs2

