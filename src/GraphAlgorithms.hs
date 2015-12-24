-- Probably better to use imperative style and state here.
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GraphAlgorithms where

import Data.Map as M
import Data.Set.Monad as S
import Automata

flattenFA :: FA q a t -> FA Int a t -- flatten states into integers
flattenFA = error "Not implemented yet"

flattenDFA :: forall q a. (Ord a, Ord q) => DFA q a -> DFA Int a
flattenDFA (FA as qs q1 (DTransition dt) fs) =
    (FA as qs2 q2 (DTransition dt2) fs2) where
    listqs :: [q]
    listqs = S.toList qs
    idx :: q -> Int
    idx q = length $ takeWhile (/= q) listqs -- the flattened state

    qs2 = S.fromList [0..(length listqs) - 1]
    q2 = idx q1
    dt2 :: M.Map (Int, a) Int
    dt2 = M.fromList $ S.toList [((idx q, a), idx (dt M.! (q,a))) | q <- qs2, a <- as]
    fs2 = S.map idx fs


-- | A rather cute algorithm for minimising DFAs that reverses them twice.
brzozowski :: (Ord a, Ord q) => DFA q a -> DFA Int a
brzozowski = flattenDFA . dReverse . dReverse . flattenDFA

-- | The reachable part of the subset construction on the reversal of a DFA.
dReverse :: (Ord a, Ord q) => DFA q a -> DFA (S.Set q) a
dReverse (FA as qs q1 (DTransition dt) fs) = 
    (FA as qs2 q2 (DTransition dt2) fs2) where
        q2 = fs -- initial state of reversed dfa is set of final states of original.
        (qs2, dt2) = process (S.singleton q2, reach (S.singleton q2))
        process (currentReached, currentTransition) =
            -- invariant: currentTransition contains
            -- all transitions from states in currentReached.
            let nextReached = S.fromList (M.elems currentTransition) in
            if currentReached == nextReached
                then (currentReached, currentTransition)
                else process (S.union currentReached nextReached,
                            M.union currentTransition
                                    (reach (S.difference nextReached currentReached)))
        reach qss = M.fromList $ S.toList [((sub, a), pre sub dt a) | sub <- qss, a <- as ]
        -- pre: states which transition into s via t on a.
        pre s t a = S.fromList [q | (q, a2) <- M.keys t, a == a2, S.member (t M.! (q, a)) s]
        fs2 = [sub | sub <- qs2, S.member q1 sub]

