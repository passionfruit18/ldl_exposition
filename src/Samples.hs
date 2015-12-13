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

newtype State = St Int deriving (Eq, Ord)
instance Show State where
    show (St i) = "State" ++ show i

class LabelState q where
    labelState :: q -> String
instance LabelState State where
    labelState (St i) = show i

as :: S.Set Alphabet
as = S.fromList $ Prelude.map L ['a','b','c']

qs :: S.Set State
qs = S.fromList $ Prelude.map St [1,2,3,4,5]

afa1 :: AFA State Alphabet
afa1 = FA as qs (St 1) (ATransition t) S.empty where
	t = M.fromList [((St 1,L 'a'), PropConst False)]
	-- 1 x a -> false

afa2 :: AFA State Alphabet
afa2 = FA as qs (St 1) (ATransition t) (S.singleton (St 3)) where
    t = M.fromList [((St 1, L 'a'), form)]
    form = mapLog id St $ And (Or (PropVar 3) (PropVar 2)) (Or (PropVar 4) (PropVar 5))
afa3 :: AFA State Alphabet
afa3 = FA as qs (St 1) (ATransition t) (S.singleton (St 3)) where
    t = M.fromList [((St 1, L 'a'), form), ((St 1, L 'b'), form2),
                    ((St 2, L 'a'), form3)]
    form = mapLog id St $ And (Or (PropVar 3) (PropVar 2)) (Or (PropVar 4) (PropVar 5))
    form2 = mapLog id St $ And (Or (PropVar 2) (PropConst True)) (Or (PropConst True) (PropConst True))
    form3 = mapLog id St $ And (Or (PropVar 3) (PropVar 4)) (Or (PropVar 1) (PropVar 2))
    

dfa1 :: DFA State Alphabet
dfa1 = FA as qs (St 1) (DTransition t) (S.map St $ S.fromList [2,3]) where
	t = M.fromList [((St 1,L 'a'), St 2),
					((St 2,L 'b'), St 1),
					((St 3,L 'c'), St 1),
					((St 1,L 'b'), St 1),
					((St 1,L 'a'), St 3)]

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

