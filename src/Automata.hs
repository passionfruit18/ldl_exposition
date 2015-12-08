{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- allows the use of list comprehension notation for monads, in particular sets

module Automata where
import qualified Data.Set.Monad as S -- Monadic Set
import qualified Data.Map as M
import Logic

-- * Types of Automata

newtype DTransition q a = 
    DTransition (M.Map (q, a) q) deriving Show
newtype NTransitionE q a =
    NTransitionE (M.Map (q, (Maybe a)) (S.Set q)) deriving Show -- ^ with epsilon-transitions
newtype NTransition q a =
    NTransition (M.Map (q, a) (S.Set q)) deriving Show
newtype ATransition q a =
    ATransition (M.Map (q, a) (BasicPropLogic q)) deriving Show -- ^ should be positive boolean formula. Todo: write function to enforce this.

-- | the general type of a finite automaton.
data FA q a t = FA {alphabet :: S.Set a, 
                    states :: S.Set q,
                    init :: q,             -- ^ just one starting state.
                    transition :: t q a,
                    final :: S.Set q} deriving Show

-- state set and alphabet implicit in transition map-- do the explicit ones match with the implicit ones?
-- That is are all key-value pairs defined for keys in alphabet X states? Not guaranteed.

type DFA q a = FA q a DTransition
type NFAE q a = FA q a NTransitionE -- ^ NFAE with alphabet "as" has transition function from (as + epsilon) x states -> ...
type NFA q a = FA q a NTransition
type AFA q a = FA q a ATransition
isFinal :: Ord q => q -> S.Set q -> Bool
isFinal = S.member

-- * Easy Translations (into more succint automata)

d2nE :: (Eq a, Ord q) =>
        DFA q a -> NFAE q a
d2nE (FA as qs q (DTransition dt) fs) = 
      (FA as qs q (NTransitionE ntE) fs) where
    ntE = (M.fromAscList .
        map (\((q1, a), q2) -> ((q1, Just a), S.singleton q2)) . -- this function must preserve asc
        -- asc being the property of ascending by key.
        M.assocs) dt -- easily create transition function

d2n :: Ord q =>
        DFA q a -> NFA q a
d2n (FA as qs q (DTransition dt) fs) =
    FA as qs q (NTransition nt) fs where
        nt = (M.map (\q -> S.singleton q) dt)

n2a :: (Eq a, Ord q) =>
        NFA q a -> AFA q a
n2a (FA as qs q (NTransition nt) fs) =
    FA as qs q (ATransition at) fs where
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
n2d (FA as qs1 q1 (NTransition nt) fs1) =
    FA as qs2 q2 (DTransition dt) fs2 where
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

