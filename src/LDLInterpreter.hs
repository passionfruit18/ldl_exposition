module LDLInterpreter(main) where

import Parsing
import LDLParser
import Environment

import Logic
import Reg
import LDLogic
import Automata

import qualified Data.Set.Monad as S
import qualified Data.Map as M
import Control.Newtype (op)

ldl_pretty_print :: LDLogic_ -> String
ldl_pretty_print = logic_pretty_print
	(\u s -> "<" ++ reg_pretty_print u ++ ">" ++ s)
	(\b s1 s2 -> "")

sep1 = take 60 (repeat '=')
sep2 = take 60 (repeat '-')
ldl_respond :: LDLogic_ -> String
ldl_respond ldl =
	let s1 = ldl_nnf_pretty_print $ ldl2nnf ldl;
		alphabet = subsets (S.fromList $ ldl_propositions ldl);
		afa = ldl2afa alphabet ldl;
		s2 = ldl2afa_pretty_print afa;
		s3 = ldl2afa2nfa_pretty_print $ a2nReach afa in
	unlines [sep1,
			"LDL: " ++ s1,
			sep2,
			"AFA: " ++ s2,
			sep2,
			"NFA: " ++ s3,
			sep1] 


reg_pretty_print :: Reg_ -> String
reg_pretty_print = reg_T_pretty_print ldl_pretty_print . op Reg
	{-foldReg basic_pretty_print
	(\f -> ldl_pretty_print f ++ "?")
	(connect "+" " ")
	(connect ";" "")
	(++"*")
	. op Reg-}

reg_nnf_pretty_print :: Show p => RegNNF p -> String
reg_nnf_pretty_print = reg_T_pretty_print ldl_nnf_pretty_print . op RegNNF
	{-foldReg basic_pretty_print
	(\f -> ldl_nnf_pretty_print f ++ "?")
	(connect "+" " ")
	(connect ";" "")
	(++"*")
	. op RegNNF-}

ldl_nnf_pretty_print :: Show p => LDLogicNNF p -> String
ldl_nnf_pretty_print = 
	logic_pretty_print
	f
	(\b s1 s2 -> "") where
		f (Diamond x) s = "<" ++ reg_nnf_pretty_print x ++ ">" ++ s
		f (Square x) s = "[" ++ reg_nnf_pretty_print x ++ "]" ++ s

fa_headings = ["Alphabet: ", "States: ", "Init: ", "Transition: ", "Final: "]
afa_pretty_print :: (Show q, Show a, Ord q, Ord a) => AFA q a -> String
afa_pretty_print (FA as qs q t fs) =
	unlines $
	zipWith (++)
	fa_headings
	[show $ ShowSet as, show $ ShowSet qs, show q, show t, show $ ShowSet fs]


ldl2afa_pretty_print :: (Show p, Ord p) => AFA (LDLogicNNF p) (S.Set p) -> String
ldl2afa_pretty_print (FA as qs q t fs) =
	let pr = ldl_nnf_pretty_print in
	unlines $
	zipWith (++)
	fa_headings
	[show $ ShowSet $ S.map ShowSet as,
	showSetWith pr qs,
	pr q,
	unlines
			$ map (\((q1,a),q2) -> (pr q1) ++
									" X " ++
									(show $ ShowSet a) ++
									" -> " ++
									(basic_pretty_print_with ((\s -> "state("++s++")") . ldl_nnf_pretty_print) q2)
									)
			$ M.assocs $ op ATransition t,
	showSetWith pr fs]

show_setOfset_with :: (Ord p) => (p -> String) -> S.Set (S.Set p) -> String
show_setOfset_with f = show . ShowSet . S.map (ShowSet . S.map f)

show_set_with :: (Ord p) => (p -> String) -> S.Set p -> String
show_set_with f = show . ShowSet . S.map f

ldl2afa2nfa_pretty_print :: (Show p, Ord p) => NFA (S.Set (LDLogicNNF p)) (S.Set p) -> String
ldl2afa2nfa_pretty_print (FA as qs q t fs) =
	let pr = ldl_nnf_pretty_print in
	unlines $
	zipWith (++)
	fa_headings
	[show $ ShowSet $ S.map ShowSet as,
	showSetOfSetWith pr qs,
	showSetWith pr q,
	unlines 
			$ map (\((q1, a), q2) -> showSetWith pr q1 ++
									" X " ++
									(show $ ShowSet a) ++
									" -> " ++
									showSetOfSetWith pr q2)
			$ M.assocs
			$ op NTransition t,
	showSetOfSetWith pr fs]

showSetWith :: Ord q => (q -> String) -> S.Set q -> String
showSetWith g qs = if S.null qs then "{}" else "{" ++ foldr1 f (map g $ S.toList qs) ++ "}"
					where f x xs = (x ++ ", " ++ xs)

showSetOfSetWith :: Ord q => (q -> String) -> (S.Set (S.Set q)) -> String
showSetOfSetWith g = showSetWith (showSetWith g)

newtype ShowSet q = ShowSet (S.Set q) deriving (Eq, Ord)
instance (Show q, Ord q) => Show (ShowSet q) where
    show (ShowSet qs) = if S.null qs then "{}" else
            "{" ++ foldr1 f (map show $ S.toList qs) ++ "}"
            where f x xs = (x ++ ", " ++ xs)


type Env = Environment Bool

init_env :: Env
init_env = make_env []

obey :: LDLogic_ -> Env -> (String, Env)
obey f env = (ldl_respond f, env)

main = dialog ldlParser obey init_env

obeyReg :: Reg_ -> Env -> (String, Env)
obeyReg reg env = (reg_pretty_print reg, env)

mainReg = dialog regParser obeyReg init_env
