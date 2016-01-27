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

ldl_respond :: LDLogic_ -> String
ldl_respond ldl =
	let s1 = ldl_pretty_print ldl;
		alphabet = subsets (S.fromList $ ldl_propositions ldl);
		afa = ldl2afa alphabet ldl;
		s2 = ldl2afa_pretty_print afa;
		s3 = show $ a2nReach afa in
	unlines ["LDL: " ++ s1,
				"AFA: " ++ s2,
				"NFA: " ++ s3] 


reg_pretty_print :: Reg_ -> String
reg_pretty_print =
	foldReg basic_pretty_print
	(\f -> ldl_pretty_print f ++ "?")
	(connect "+")
	(connect ";")
	(++"*")
	. op Reg

reg_nnf_pretty_print :: Show p => RegNNF p -> String
reg_nnf_pretty_print =
	foldReg basic_pretty_print
	(\f -> ldl_nnf_pretty_print f ++ "?")
	(connect "+")
	(connect ";")
	(++"*")
	. op RegNNF
ldl_nnf_pretty_print :: Show p => LDLogicNNF p -> String
ldl_nnf_pretty_print = 
	foldLog show show -- propositional constants and variables
	("!"++) -- not
	(connect "&&")
	(connect "||")
	f
	(\b s1 s2 -> "") where
		f (Diamond x) s = "<" ++ reg_nnf_pretty_print x ++ ">" ++ s
		f (Square x) s = "[" ++ reg_nnf_pretty_print x ++ "]" ++ s

afa_pretty_print :: (Show q, Show a, Ord q, Ord a) => AFA q a -> String
afa_pretty_print (FA as qs q t fs) =
	unlines $
	zipWith (++)
	["Alphabet: ", "States: ", "Init: ", "Transition: ", "Final: "]
	[show $ ShowSet as, show $ ShowSet qs, show q, show t, show $ ShowSet fs]

ldl2afa_pretty_print :: (Show p, Ord p) => AFA (LDLogicNNF p) (S.Set p) -> String
ldl2afa_pretty_print (FA as qs q t fs) =
	unlines $
	zipWith (++)
	["Alphabet: ", "States: ", "Init: ", "Transition: ", "Final: "]
	[show $ ShowSet $ S.map ShowSet as,
	show $ ShowSet $ S.map ldl_nnf_pretty_print qs,
	ldl_nnf_pretty_print q,
	unlines $ map show $ map (\((q1,a),q2) -> (ldl_nnf_pretty_print q1) ++ " X " ++ (show $ ShowSet a) ++ " -> " ++ (show q2)) $ M.assocs $ op ATransition t,
	show $ ShowSet $ S.map ldl_nnf_pretty_print fs]


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
