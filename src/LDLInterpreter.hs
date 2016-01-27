module LDLInterpreter(main) where

import Parsing
import LDLParser
import Environment

import Logic
import Reg
import LDLogic
import Automata

import qualified Data.Set.Monad as S
import Control.Newtype (op)

ldl_pretty_print :: LDLogic_ -> String
ldl_pretty_print = logic_pretty_print_
	(\u s -> "<" ++ reg_pretty_print u ++ ">" ++ s)
	(\b s1 s2 -> "")

ldl_respond :: LDLogic_ -> String
ldl_respond ldl =
	let s1 = ldl_pretty_print ldl;
		alphabet = subsets (S.fromList $ ldl_propositions ldl);
		afa = ldl2afa alphabet ldl;
		s2 = show afa;
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

basic_pretty_print :: BasicPropLogic_ -> String
basic_pretty_print = logic_pretty_print_
	(\u s -> "")
	(\b s1 s2 -> "")

type Env = Environment Bool

init_env :: Env
init_env = make_env []

obey :: LDLogic_ -> Env -> (String, Env)
obey f env = (ldl_respond f, env)

main = dialog ldlParser obey init_env

obeyReg :: Reg_ -> Env -> (String, Env)
obeyReg reg env = (reg_pretty_print reg, env)

mainReg = dialog regParser obeyReg init_env
