{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

-- allows the use of list comprehension notation for monads, in particular sets

module Automata where
import qualified Data.Set.Monad as S -- Monadic Set
import qualified Data.Map as M
import Logic
import LDLogic
import Reg
import Control.Newtype

-- * Helper function

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

instance Newtype (DTransition q a) (M.Map (q, a) q) where
    pack = DTransition
    unpack (DTransition x) = x
instance Newtype (NTransitionE q a) (M.Map (q, (Maybe a)) (S.Set q)) where
    pack = NTransitionE
    unpack (NTransitionE x) = x
instance Newtype (NTransition q a) (M.Map (q, a) (S.Set q)) where
    pack = NTransition
    unpack (NTransition x) = x
instance Newtype (ATransition q a) (M.Map (q, a) (BasicPropLogic q)) where
    pack = ATransition
    unpack (ATransition x) = x
instance Ord k => Newtype (M.Map k e) [(k,e)] where
    pack = M.fromList
    unpack = M.assocs
-- | the general type of a finite automaton.
data FA q a t = FA {alphabet :: S.Set a, 
                    states :: S.Set q,
                    init :: q,             -- ^ just one starting state.
                    transition :: t q a,
                    final :: S.Set q}
instance (Ord q, Ord a, Show q, Show a, Show (t q a)) => Show (FA q a t) where
    show (FA as qs q t fs) = unlines [
        "Alphabet: " ++ show as,
        "States: " ++ show qs,
        "Init: " ++ show q,
        "Transition: " ++ show t,
        "Final: " ++ show fs]

-- state set and alphabet implicit in transition map-- do the explicit ones match with the implicit ones?
-- That is are all key-value pairs defined for keys in alphabet X states? Not guaranteed.

type DFA q a = FA q a DTransition
type NFAE q a = FA q a NTransitionE -- ^ NFAE with alphabet "as" has transition function from (as + epsilon) x states -> ...
type NFA q a = FA q a NTransition
type AFA q a = FA q a ATransition
isFinal :: Ord q => q -> S.Set q -> Bool
isFinal = S.member

-- * Easy Translations (lifting up to higher level automata)

d2nE :: (Ord a, Ord q) =>
        DFA q a -> NFAE q a
d2nE (FA as qs q dt fs) = 
      (FA as qs q ntE fs) where
        ntE = (over DTransition $
                over M.fromList $
                map (\((q1, a), q2) -> ((q1, Just a), S.singleton q2))
                )
                dt

d2n :: Ord q =>
        DFA q a -> NFA q a
d2n (FA as qs q dt fs) =
    FA as qs q nt fs where
        nt = over DTransition (M.map (\q -> S.singleton q)) dt

n2a :: (Ord a, Ord q) =>
        NFA q a -> AFA q a
n2a (FA as qs q nt fs) =
    FA as qs q at fs where
        at = (over NTransition $
                over M.fromList $
                map (\((q1, a), setQ) -> ((q1, a), bigOr setQ))
                )
                nt

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


setSatBasicProp :: Ord p => S.Set p -> BasicPropLogic p -> Bool
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

ldl2afa :: forall q p a. Ord p => S.Set (S.Set p) -> LDLogic p -> AFA (LDLogicNNF p) (S.Set p)
-- states are the Fischer-Ladner closure of the given formula,
-- and the alphabet comprises sets of "p". i.e. Propositional assignments over p.
ldl2afa assigns ldl =
    (FA assigns qs q (ATransition at) fs) where
        q = ldl2nnf ldl
        (qs, at) = reach (S.singleton q) postMap elemsReached
        postMap qs = set2map [((ldlN_, assign), delta assign ldlN_) | ldlN_ <- qs, assign <- assigns] -- is it a problem to use ldl again?
        -- Usually we would have delta :: States X Alphabet -> PosBool States,
        -- but I switched the arguments to take advantage of currying.
        
        elemsReached t = S.fromList $ concat $ map collapse $ (M.elems t)
        -- collapse the BasicPropLogic formulae (which have LDLNNF formulae as propositions) into lists of LDL formulae
        collapse :: BasicPropLogic (LDLogicNNF p) -> [LDLogicNNF p]
        collapse = foldLog (\c -> []) (\f -> [f]) id (++) (++) (error "Unary") (error "Binary")
        -- first id is for PropVars, second is for negation
        fs = S.empty

type BP_LDLogicNNF p = BasicPropLogic (LDLogicNNF p)
type BoolOp p = BP_LDLogicNNF p -> BP_LDLogicNNF p -> BP_LDLogicNNF p

delta :: forall p. Ord p => S.Set p -> LDLogicNNF p -> BP_LDLogicNNF p
delta assign = da where
    da :: LDLogicNNF p -> BP_LDLogicNNF p
    da (PropConst b) = PropConst b
    da (PropVar lit) = pr lit where
        pr (Positive p) = PropConst (S.member p assign)
        pr (Negative p) = PropConst (not $ S.member p assign)
    da (Not x) = (error "negation in NNF formula")
    da (And f1 f2) = And (da f1) (da f2)
    da (Or f1 f2) = Or (da f1) (da f2)
    da (Unary (Diamond reg) f) = urf True f reg
    da (Unary (Square reg) f) = urf False f reg
    da (Binary _ _ _) = error "no binary modality"
    -- urf: Unary Regex modality Formula translation
    urf :: Bool -> LDLogicNNF p -> RegNNF p -> BP_LDLogicNNF p
    urf k f (RegNNF x) = g x where
        bf1_ :: BoolOp p
        bf2_ :: BoolOp p
        m_ :: Reg_T p (LDLogicNNF p) -> DOS p
        g :: Reg_T p (LDLogicNNF p) -> BP_LDLogicNNF p
        (b_, bf1_, bf2_, m_) = -- boolean, boolean function 1, 2, modality
            if k then (False, And, Or, Diamond . RegNNF)
                else (True, Or, And, Square . RegNNF)
        g (Base basicf) =
            if (setSatBasicProp assign basicf) then PropVar f else PropConst b_
        g (Test l) = bf1_ (da l) (da f)
        g (Plus r1 r2) = bf2_ (g r1) (g r2)
        g (Comp r1 r2) = da (Unary (m_ r1) $ Unary (m_ r2) f)
        g (Star r) = if test_only r then da f else bf2_ (da f) (da $ Unary (m_ r) $ Unary (m_ (Star r)) f)


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

afaEmpty :: (Ord q, Ord a) => AFA q a -> Bool
afaEmpty afa = let (FA as qs q (NTransition nt) fs) = a2nReach afa in
                S.null fs