module LDLInterpreter(main) where
import Parsing
import LDLParser
import Logic
import Environment

ldl_pretty_print :: LDLogic_ -> String
ldl_pretty_print = logic_pretty_print_
	(\u s -> "<" ++ reg_pretty_print u ++ ">" ++ s)
	(\b s1 s2 -> "")

reg_pretty_print :: Reg_ -> String
reg_pretty_print =
	foldReg basic_pretty_print
	(\f -> ldl_pretty_print f ++ "?")
	(connect "+")
	(connect ";")
	(++"*")

basic_pretty_print :: BasicPropLogic_ -> String
basic_pretty_print = logic_pretty_print_
	(\u s -> "")
	(\b s1 s2 -> "")

type Env = Environment Bool

init_env :: Env
init_env = make_env []

obey :: LDLogic_ -> Env -> (String, Env)
obey f env = (ldl_pretty_print f, env)

main = dialog ldlParser obey init_env

obeyReg :: Reg_ -> Env -> (String, Env)
obeyReg reg env = (reg_pretty_print reg, env)

mainReg = dialog regParser obeyReg init_env
