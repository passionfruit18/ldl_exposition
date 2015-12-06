module LDLInterpreter(main) where
import Parsing
import LDLParser
import Logic
import Environment

ldl_pretty_print :: LDLogic_ -> String
ldl_pretty_print = logic_pretty_print_
	(\u s -> "<" ++ prog_pretty_print u ++ ">" ++ s)
	(\b s1 s2 -> "")

prog_pretty_print :: Prog_ -> String
prog_pretty_print =
	foldProg basic_pretty_print
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

obeyProg :: Prog_ -> Env -> (String, Env)
obeyProg prog env = (prog_pretty_print prog, env)

mainProg = dialog progParser obeyProg init_env

{-
> foldLog cr pr nr ar orr ur br log = f log where
>     f (PropConst c) = cr c
>     f (PropVar p) = pr p
>     f (Not log) = nr $ f log
>     f (And log1 log2) = ar (f log1) (f log2)
>     f (Or log1 log2) = orr (f log1) (f log2)
>     f (Unary u log) = ur u (f log)
>     f (Binary b log1 log2) = br b (f log1) (f log2)

> type LDLogic p = PropLogic p (Prog p) Void
> data Prog p =
>   Base (BasicPropLogic p) |
>   Test (LDLogic p) |
>   Plus (Prog p) (Prog p) |
>   Comp (Prog p) (Prog p) |
>   Star (Prog p)
> foldProg basic ldl prr crr sr prog = f prog where
>	f (Base b) = basic b
>	f (Test f) = ldl f
>	f (Plus p1 p2) = prr (f p1) (f p2)
>	f (Comp p1 p2) = crr (f p1) (f p2)
>	f (Star p) = sr (f p)
> 	
-}