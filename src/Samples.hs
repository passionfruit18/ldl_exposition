{-# LANGUAGE MonadComprehensions #-}
module Samples where
import Automata
import Logic
import Data.Set.Monad as S
import Data.Map as M
import Prelude as P


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

-- show is for making the state into a white-space-free identifier
-- used inside the AFA drawing algorithm.
-- labelState is what will visibly show up on the graph, labelled on each node.

mkAlph :: String -> S.Set Alphabet
mkAlph cs = S.fromList $ P.map L cs

qs :: S.Set State
qs = mkStates [1,2,3,4,5]

mkStates :: [Int] -> S.Set State
mkStates ns = S.fromList $ P.map St ns


afa1 :: AFA State Alphabet
afa1 = FA (mkAlph "a") qs (St 1) (ATransition t) S.empty where
	t = M.fromList [((St 1,L 'a'), PropConst False)]
	-- 1 x a -> false

afa2 :: AFA State Alphabet
afa2 = FA (mkAlph "a") qs (St 1) (ATransition t) (S.singleton (St 3)) where
    t = M.fromList [((St 1, L 'a'), form)]
    form = mapLog id St $ And (Or (PropVar 3) (PropVar 2)) (Or (PropVar 4) (PropVar 5))
afa3 :: AFA State Alphabet
afa3 = FA (mkAlph "ab") qs (St 1) (ATransition t) (S.singleton (St 3)) where
    t = M.fromList [((St 1, L 'a'), form), ((St 1, L 'b'), form2),
                    ((St 2, L 'a'), form3)]
    form = mapLog id St $ And (Or (PropVar 3) (PropVar 2)) (Or (PropVar 4) (PropVar 5))
    form2 = mapLog id St $ And (Or (PropVar 2) (PropConst True)) (Or (PropConst True) (PropConst True))
    form3 = mapLog id St $ And (Or (PropVar 3) (PropVar 4)) (Or (PropVar 1) (PropVar 2))

nfa1 :: NFA State Alphabet
nfa1 = FA (mkAlph "a") qs (St 1) (NTransition t) (S.singleton (St 3)) where
    t = M.fromList [((St 1, L 'a'), mkStates [1,2,3,4])]

dfa1 :: DFA State Alphabet
dfa1 = FA (mkAlph "abc") qs (St 1) (DTransition t) (mkStates [2,3]) where
	t = M.fromList [((St i, L c), St j) |
                        (i,c,j) <- [(1, 'a', 2),
                                    (2, 'b', 1),
                                    (3, 'c', 1),
                                    (1, 'b', 1),
                                    (1, 'a', 3)]]

newtype ShowSet q = ShowSet (S.Set q) deriving (Eq, Ord)
instance (Show q, Ord q) => Show (ShowSet q) where
    show (ShowSet qs) = "Set" ++ (concat $ P.map show (S.toList qs))

instance (LabelState q, Ord q) => LabelState (ShowSet q) where
    labelState (ShowSet qs) =
        if S.null qs then "{}" else
            "{" ++ foldr1 f (P.map labelState $ S.toList qs) ++ "}"
            where f x xs = (x ++ ", " ++ xs)

dfa2 :: DFA (ShowSet State) Alphabet
dfa2 = dfaMap ShowSet (n2d nfa1)

dfaMap :: (Ord q, Ord r, Ord a) => (q -> r) -> DFA q a -> DFA r a
dfaMap f (FA as qs1 q1 (DTransition dt1) fs1) =
    (FA as qs2 q2 (DTransition dt2) fs2) where
        qs2 = S.map f qs1
        q2 = f q1
        fs2 = S.map f fs1
        dt2 = M.fromAscList $
            (\ts -> [((f q, a), f q') | ((q, a), q') <- ts]) $
             M.toAscList dt1
{-
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
-}

