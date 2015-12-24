{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- allows the use of list comprehension notation for monads, in particular sets

module AutomataQuery where
import qualified Data.Set.Monad as S -- Monadic Set
import qualified Data.Map as M
import Logic

-- * Types of Automata

type Transition q a r = M.Map (q,a) r

-- | the general type of a realised finite automaton.
data FA q a r = FA {alphabet :: S.Set a, 
                    states :: S.Set q,
                    init :: q,             -- ^ just one starting state.
                    transition :: Transition q a r,
                    final :: S.Set q} deriving Show

-- state set and alphabet implicit in transition map-- do the explicit ones match with the implicit ones?
-- That is are all key-value pairs defined for keys in alphabet X states? Not guaranteed.

newtype DFA q a = DFA (FA q a q)
newtype NFAE q a = NFAE (FA q (Maybe a) (S.Set q)) -- ^ NFAE with alphabet "as" has transition function from (as + epsilon) x states -> ...
newtype NFA q a = NFA (FA q a (S.Set q))
newtype AFA q a = AFA (FA q a (BasicPropLogic q))
isFinal :: Ord q => q -> S.Set q -> Bool
isFinal = S.member

-- * Query-Automata

class (FAquery q a r) m where
    alphaQ :: a -> m -> Bool
    stateQ :: q -> m -> Bool
    initQ :: m -> q
    transitionQ :: q -> a -> m -> (Maybe r)
    finalQ :: q -> m -> Bool

instance (FAquery q a r) (FA q a r) where
    alphaQ a fa = S.member a (alphabet fa)
    stateQ q fa = S.member q (states fa)
    initQ fa = (init fa)
    transitionQ q a fa = M.lookup (q,a) (transition fa)
    finalQ q fa = S.member q (final fa)


realise :: (FAquery q a r) m =>
        m -> (S.Set a) -> (Transition q a r -> S.Set q)
        -> (FA q a r)
realise m as elemsReached = FA as qs q0 t fs where
    q0 = initQ m
    (qs, t) = reach (S.singleton q0) postMap elemsReached
    postMap sub = M.fromList $ S.toList [((q, a), r) | q <- sub, a <- as, (Just r) <- transitionQ q a m]-- what as
    fs = S.filter (flip finalQ m) qs


-- * Easy Translations (into more succint automata)

d2nE :: (Eq a, Ord q) =>
        DFA q a -> NFAE q a
d2nE (DFA (FA as qs q (Transition dt) fs)) = 
      (NFAE (FA (S.map Just as) qs q (Transition ntE) fs)) where
    ntE = (M.fromAscList .
        map (\((q1, a), q2) -> ((q1, Just a), S.singleton q2)) . -- this function must preserve asc
        -- asc being the property of ascending by key.
        M.assocs) dt -- easily create transition function

d2n :: Ord q =>
        DFA q a -> NFA q a
d2n (DFA (FA as qs q (DTransition dt) fs)) =
    (NFA (FA as qs q (NTransition nt) fs) where
        nt = (M.map (\q -> S.singleton q) dt)

n2a :: (Eq a, Ord q) =>
        NFA q a -> AFA q a
n2a (NFA (FA as qs q (NTransition nt) fs)) =
    (AFA (FA as qs q (ATransition at) fs) where
    at = (M.fromAscList .
            map (\((q1, a), setQ) -> ((q1, a), bigOr setQ)) . -- must preserve asc
            M.assocs) nt

bigOr :: (Ord p, Ord u, Ord b) =>
            S.Set p -> PropLogic p u b
bigOr = S.foldr Or (PropConst False) . S.map PropVar

-- * Harder Translations (compiling into lower level automata)

-- ** NFAs to DFAs

nE2n :: NFAE q a -> NFA q a
nE2n = error "Not implemented yet!"

n2d :: (Ord a, Ord q) =>
        NFA q a -> DFA (S.Set q) a
n2d = n2dNaive

n2dNaive :: (Ord a, Ord q) =>
        NFA q a -> DFA (S.Set q) a
n2dNaive (NFA (FA as qs1 q1 (NTransition nt) fs1)) =
    DFA (FA as qs2 q2 (DTransition dt) fs2) where
    q2 = S.singleton q1
    qs2 = subsets qs1 -- subset construction
    dt = M.fromList $ S.toList $ -- convert set to list to map
          do
            sub <- qs2 -- monadic usage of set
            a <- as
            return ((sub, a), post sub nt a)
    fs2 = S.filter (\sub -> not $ S.null $ S.intersection sub fs1) qs2

post :: (Ord a, Ord q) =>
        S.Set q -> M.Map (q,a) (S.Set r) -> a -> S.Set r
-- ^ states reachable from set s according to t, via action a
post s t a = s >>= \q -> t M.! (q,a)

n2dReach :: (Ord a, Ord q) =>
        NFA q a -> DFA (S.Set q) a
n2dReach (NFA (FA as qs1 q1 (Transition nt) fs1)) =
    DFA (FA as qs2 q2 (Transition dt) fs2) where
        q2 = S.singleton q1
        (qs2, dt) = reach (S.singleton q2) postMap elemsReached
        postMap qs =
            M.fromList $ S.toList
            [((sub, a), sub >>= \q -> nt M.! (q,a)) | sub <- qs, a <- as]
        elemsReached t = S.fromList (M.elems t)


reach :: (Eq q) => S.Set q ->
            (S.Set q -> Transition q a r) ->
            (Transition q a r-> S.Set q) -> 
            (S.Set q, Transition q a r)
reach s0 postMap elemsReached = reach' (s0, postMap s0) where
    reach' (currentReached, currentTransition) =
        let nextReached = elemsReached currentTransition in
        if currentReached == nextReached
            then (currentReached, currentTransition)
            else reach' (S.union currentReached nextReached,
                        M.union currentTransition
                                (postMap (S.difference nextReached currentReached)))

subsets :: Ord a =>
            S.Set a -> S.Set (S.Set a)
-- ^ powerset
subsets = S.foldr
          (\a subs -> S.union subs
                      (S.map (S.insert a) subs))
          (S.singleton S.empty)

-- ** AFAs to NFAs

a2n :: (Ord a, Ord q) =>
        AFA q a -> NFA (S.Set q) a
a2n (FA as qs1 q1 (ATransition at) fs1) =
    FA as qs2 q2 (NTransition nt) fs2 where
    q2 = S.singleton q1
    qs2 = subsets qs1
    nt = M.fromList $
         S.toList $
          [((sub0, a), minSat qs1
            -- nt (sub0, a) maps to subsets sub1 of qs which satisfy all ... 
            -- boolean formulas reached from a state q of sub0 via a on at. 
            (\sub1 -> and $ S.toList $
             [satisfy sub1 (at M.! (q,a)) | q <- sub0]))
           -- nt :: P(Q) X A -> P(P(Q))
           | sub0 <- qs2, a <- as]
    satisfy s l = satisfyBasicProp (flip S.member s) l
    fs2 = [ sub | sub <- qs2, S.isSubsetOf sub fs1 ]

minSat :: forall q. Ord q =>
        S.Set q -> (S.Set q -> Bool) -> S.Set (S.Set q)
minSat total pred = -- minimal subsets of total satisfying monotonic pred
    if not (pred total) then S.empty else
        S.fromList $ process [total] where
            process :: [S.Set q] -> [S.Set q]
            process [] = []
            process (sub:subs) = 
                let lesserSubs = explode sub in
                if lesserSubs == [] then
                    sub : (process $ filter (not . (S.isSubsetOf sub)) subs) -- return sub,
                    -- remove supersets of sub since it's minimal
                else process (lesserSubs ++ subs) -- hits a base case in that explode S.empty = []
            explode :: S.Set q -> [S.Set q]
            explode sub = filter pred $ S.toList $ S.map (\q -> S.delete q sub) sub


-- * LDL to AFAs
ldl2afa :: LDLogic p -> AFA (LDLogic p) (S.Set p) -- states Fischer-Ladner closure and alphabet 2^P
ldl2afa = error "Not implemented yet"

-- * Emptiness testing

dfaEmpty :: DFA q a -> Bool
dfaEmpty = error "Not implemented yet"

