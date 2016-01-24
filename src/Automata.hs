{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- allows the use of list comprehension notation for monads, in particular sets

module Automata where
import qualified Data.Set.Monad as S -- Monadic Set
import qualified Data.Map as M
import Logic

set2map :: (Ord k, Ord e) => S.Set (k, e) -> M.Map k e
set2map = M.fromList . S.toList

-- * Types of Automata

newtype DTransition q a = 
    DTransition (M.Map (q, a) q) deriving Show
-- |with epsilon-transitions
newtype NTransitionE q a =
    NTransitionE (M.Map (q, (Maybe a)) (S.Set q)) deriving Show

newtype NTransition q a =
    NTransition (M.Map (q, a) (S.Set q)) deriving Show
-- |should be positive boolean formula. Todo: write function to enforce this.
newtype ATransition q a =
    ATransition (M.Map (q, a) (BasicPropLogic q)) deriving Show 
    
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

n2d :: (Ord a, Ord q) => NFA q a -> DFA (S.Set q) a
n2d = n2dReach
n2dNaive :: (Ord a, Ord q) => NFA q a -> DFA (S.Set q) a
n2dNaive (FA as qs1 q1 (NTransition nt) fs1) =
    FA as qs2 q2 (DTransition dt) fs2 where
    q2 = S.singleton q1
    qs2 = subsets qs1 -- subset construction
    dt = set2map $ -- convert set to list to map
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

-- | Not so naive n2d transformation, getting only the reachable part
-- and also handling the transition function having no entry for a given key.
n2dReach :: (Ord a, Ord q) => NFA q a -> DFA (S.Set q) a
n2dReach (FA as qs1 q1 (NTransition nt) fs1) =
    (FA as qs2 q2 (DTransition dt) fs2) where
        q2 = S.singleton q1
        (qs2, dt) = reach (S.singleton q2) postMap elemsReached
        postMap qs =
            set2map
            [((sub, a), sub >>= \q -> setLookup (q,a) nt) | sub <- qs, a <- as]
        elemsReached t = S.fromList (M.elems t)
        fs2 = S.filter (\sub -> not $ S.null $ S.intersection sub fs1) qs2
setLookup :: (Ord q, Ord a, Ord r) => (q,a) -> M.Map (q,a) (S.Set r) -> S.Set r
setLookup k t =
    case M.lookup k t of
        Just s -> s
        Nothing -> S.empty
type Transition q a r = M.Map (q,a) r

-- | reach takes a set s0 of type Set q, function postMap :: Set q -> Transition q a r,
-- function elemsReached of type Transition q a r -> S.Set q, and gives the pair (qs, t)
-- such that qs is 'closed' under (elemsReached . postMap) and t = postMap qs.
-- postMap qs should give the transitions from states in qs.
-- elemsReached t should somehow transform the image of t into a set of elements.
reach :: (Ord q, Ord a) => S.Set q ->
            (S.Set q -> Transition q a r) ->
            (Transition q a r-> S.Set q) -> 
            (S.Set q, Transition q a r)
reach s0 postMap elemsReached = reach' (s0, postMap s0) where
    -- invariant: currentTransition = postMap currentReached
    reach' (currentReached, currentTransition) =
        let nextReached = elemsReached currentTransition in
        if (S.isSubsetOf nextReached currentReached)
            then (currentReached, currentTransition)
            else reach' (S.union currentReached nextReached,
                        M.union currentTransition
                                (postMap (S.difference nextReached currentReached)))


-- ** AFAs to NFAs

a2n :: (Ord a, Ord q) => AFA q a -> NFA (S.Set q) a
a2n = a2nReach

a2nNaive :: (Ord a, Ord q) => AFA q a -> NFA (S.Set q) a
a2nNaive (FA as qs1 q1 (ATransition at) fs1) =
    FA as qs2 q2 (NTransition nt) fs2 where
    q2 = S.singleton q1
    qs2 = subsets qs1
    nt = set2map $
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
minSat total pred = -- minimal subsets of "total" set that satisfy the monotonic pred(icate)
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


setSatBasicProp :: S.Set p -> BasicPropLogic p -> Bool
setSatBasicProp s l = satisfyBasicProp (flip S.member s) l

a2nReach :: (Ord a, Ord q) => AFA q a -> NFA (S.Set q) a
a2nReach (FA as qs1 q1 (ATransition at) fs1) =
    FA as qs2 q2 (NTransition nt) fs2 where
    q2 = S.singleton q1 -- NFA states are sets of AFA states
    (qs2, nt) = reach (S.singleton q2) postMap elemsReached
    postMap qs =
        set2map
        [((sub0, a), minSat qs1
            (\sub1 -> and $ S.toList $
                [satisfy sub1 (M.lookup (q,a) at) | q <- sub0]))
        | sub0 <- qs, a <- as]
    elemsReached t = S.unions (M.elems t)
    satisfy s Nothing = False -- False if lookup fails. Nothing "is" false, and no assignment satisfies false.
    satisfy s (Just l) = setSatBasicProp s l
    fs2 = [ sub | sub <- qs2, S.isSubsetOf sub fs1 ]

-- * LDL to AFAs

ldl2afa :: S.Set (S.Set p) -> LDLogic p -> AFA (LDLogic p) (S.Set p)
-- states are the Fischer-Ladner closure of the given formula,
-- and the alphabet comprises sets of "p". i.e. Propositional assignments over p.
ldl2afa assigns ldl = let ldlN = ldl2nnf ldl in
    FA (assigns qs q (ATransition at) fs) where
        q = ldlN
        (qs, at) = reach (S.singleton q) postMap elemsReached
        postMap qs = set2map [((ldlN_, assign), delta assign ldlN_) | ldlN_ <- qs, assign <- assigns] -- is it a problem to use ldl again?
        -- Usually we would have delta :: States X Alphabet -> PosBool States,
        -- but I switched the arguments to take advantage of currying.
        
        elemsReached t = concat (map collapse) (M.elems t)
        -- collapse the BasicPropLogic formulae (which have LDLNNF formulae as propositions) into lists of LDL formulae
        collapse = foldLog (\c -> []) id id (++) (++) (error "Unary") (error "Binary")
        -- first id is for PropVars, second is for negation
        fs = S.empty
delta :: S.Set p -> LDLogicNNF p -> BasicPropLogic (LDLogicNNF p)
delta assign = da where
    da (PropConst b) = PropConst b
    da (PropVar lit) = pr lit where
        pr (Positive p) = PropConst (S.member p assign)
        pr (Negative p) = PropConst (not $ S.member p assign)
    da (Not x) = (error "negation in NNF formula")
    da (And f1 f2) = And (delta assign f1) (delta assign f2)
    da (Or f1 f2) = Or (delta assign f1) (delta assign f2)
    da (Unary (Diamond reg) f) = urf True f reg
    da (Unary (Square reg) f) = urf False f reg
    urf k f = g where -- boolean, boolean function 1, 2, modality
        (b, bf1, bf2, m) =
            if k then (False, And, Or, Diamond)
                else (True, Or, And, Square)
        g (Base basicf) =
            if (setSatBasicProp assign basicf) then f else b
        g (Test l) = error (bf1 (da l) (da f)) -- but l is LDLogic, not LDLogicNNF, by the type of Reg...
        g (Plus r1 r2) = bf2 (g r1) (g r2)
        g (Comp r1 r2) = da (Unary (m r1) $ Unary (m r2) f)
        g (Star r) = if test_only r then da f else bf2 (da f) (da $ Unary (m1 r) $ Unary (m1 (Star r)) f)
    da (Binary _ _ _) = error "no binary modality"

{-
I have to make a choice. The double recursiveness makes it difficult to introduce a variant
of LDLogic, namely LDLogicNNF, since the Reg given to LDLogicNNF refers back to LDLogic.
I don't know if Haskell has any fancy features to paramaterise the back-reference.
In absence of that, either I have to make a RegNNF, or I have to expand LDLogic so that it can have
negation normal form materialised within it (i.e. I can do away with the "Not"s).
If I make a RegNNF, I'll also have to make lots of helper functions.
If I expand LDLogic, it might break other things...
I suppose I'll have to take a break here.
-}

-- * Emptiness testing

dfaEmpty :: DFA q a -> Bool
dfaEmpty = error "Not implemented yet"

afaEmpty :: AFA q a -> Bool
afaEmpty afa = let (FA as qs q (NTransition nt) fs) = a2nReach afa in
                S.empty fs