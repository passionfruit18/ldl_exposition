-- Probably better to use imperative style and state here.
module GraphAlgorithms where

import Data.Map as M
import Data.Set.Monad as S
import Automata
{-}
flattenFA :: FA q a t -> FA Int a t -- flatten states into integers
flattenFA = error "Not implemented yet"

flattenDFA :: DFA q a -> DFA Int a
flattenDFA (FA as qs q1 (DTransition dt) fs) =
    (FA as qs2 q2 (DTransition dt2) fs2) where
    listqs = S.toList qs
    index q = length $ takeWhile (/= q) listqs -- the flattened state

    qs2 = [0..(length listqs) - 1]
    q2 = index q1
    dt2 = M.fromList [((q, a), index (dt M.! (q,a))) | q <- qs2, a <- as]
    fs2 = S.map index fs


brzozowski :: DFA q a -> DFA Int a
brzozowski = flattenDFA . dReverse . dReverse . flattenDFA

dReverse :: DFA q a -> DFA (S.Set q) a
dReverse (FA as qs q1 (DTransition dt) fs) = 
    (FA as (S.fromList qs2) q2 (DTransition dt2) fs2) where
        qs2 = q2 : reachableFrom q2
        q2 = fs -- initial state of reversed dfa is set of final states of original.
        dt2 = M.fromList [((sub, a), pre sub dt a) -- take the preimage.
                            | sub <- qs2, a <- as]
        fs2 = S.fromList [sub | sub <- qs2, S.member q1 sub]
        reachableFrom q = fixYield f (S.singleton q) (f (S.singleton q)) where
            f qs = [dt2 M.! (q,a) | q <- qs, a <- as]
            fixYield f s1 s2 = -- s1 subset of s2, f nondecreasing
                if s1 == s2 then []
                    else S.toList (S.difference s2 s1) ++ (fixYield f s2 (f s2))

-- I am trying to be clever and construct qs2 and dt2 mutually recursively, relying on laziness.
-- only I am not sure the laziness would get through the Map and Set datatypes conversions,
-- or whether those conversions are strict.
-- Well, perhaps I can only do this iteratively.

pre :: (Ord a, Ord q) =>
        S.Set r -> M.Map (q,a) r -> a -> S.Set q
-- states which transition into s via t on a.
pre s t a = S.fromList [q | (q, a2) <- M.keys t, a == a2, S.member (t M.! (q, a)) s]
-}
