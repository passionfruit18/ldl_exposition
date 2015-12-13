-- based on Mike Spivey's FunParser from the POPL course
module LDLParser(ldlParser, regParser, lexer, LDLogic_, Reg_, BasicPropLogic_) where
import Parsing
import Logic -- this is the abstract syntax which we want to parse into.
import Data.Char

-- this defines the concrete syntax for LDL in a way that can be parsed.

type Proposition = String
type LDLogic_ = LDLogic Proposition
type Reg_ = Reg Proposition
type BasicPropLogic_ = BasicPropLogic Proposition

data Token = 
	PROP Proposition -- alphanumeric strings used for propositions
	| CONST Bool
	| LPAR | RPAR -- parentheses
	| NOT | AND | OR -- | THEN | EQUIV | -- logical operators
	| LTRI | RTRI | BRA | KET -- <, >, [, ] brackets for modalities
	| TEST | PLUS | SEMI | STAR -- ?, +, ;, * regular expressions for modalities
	| BADTOK Char deriving Eq


data IdKind =
	MONLOGOP | LOGOP | REGOP | POSTREGOP deriving (Eq, Show)
	-- not sure if I'll need this.

kwlookup = make_kwlookup PROP [("true", CONST True), ("false", CONST False)]
instance Show Token where
	show t =
		case t of
			PROP c -> show c
			CONST b -> show b
			LPAR -> "("; RPAR -> ")"
			NOT -> "!"; AND -> "&&"; OR -> "||" -- THEN -> "->"; EQUIV -> "<->"
			-- the use of < and > occurs for multiple tokens...
			LTRI -> "<"; RTRI -> ">"; BRA -> "["; KET -> "]"
			TEST -> "?"; PLUS -> "+"; SEMI -> ";"; STAR -> "*"; BADTOK c -> [c]

lexer :: Lexer Token
lexer =
	do
		c <- nextch;
		case c of
			_ | isAlpha c ->
				(do
					s <- star (\c -> isAlphaNum c || c == '_');
					return (kwlookup (c:s)))
			'(' -> return LPAR; ')' -> return RPAR
			'!' -> return NOT
			'&' -> switch [('&', return AND)] (return (BADTOK c))
			'|' -> switch [('|', return OR)] (return (BADTOK c))
			'<' -> return LTRI; '>' -> return RTRI
			'[' -> return BRA; ']' -> return KET
			'?' -> return TEST;'+' -> return PLUS;
			';' -> return SEMI; '*' -> return STAR
			' ' -> lexer
			'\t' -> lexer
			'\n' -> do incln; lexer
			_ -> return (BADTOK c)


p_chainr :: (a -> b -> b -> b) -> Parser t a -> Parser t b -> Parser t b
p_chainr mk p_op p_rand =
	do f1 <- p_rand; p_tail f1
	where
		p_tail f1 =
			do w <- p_op; f2 <- p_chainr mk p_op p_rand;
					return (mk w f1 f2)
			<+> return f1

p_ldlFormula :: Parser Token LDLogic_
-- precedence: weakest to strongest (occurring first to last)
-- f || f, f && f, !f, <reg>f
p_ldlFormula =
	dnf p_ldlTerm1

dnf :: Parser Token (PropLogic p u b) -> Parser Token (PropLogic p u b) -- disjunction of conjunction of ...
-- applies operator precedence of && binding more tightly than ||
dnf p = p_disj where
	p_disj = p_chainr (\OR -> Or) (eat OR) p_conj
	p_conj = p_chainr (\AND -> And) (eat AND) p

p_ldlTerm1 = -- chain of unary operators !f, <>f, []f.
	do eat NOT; f <- p_ldlTerm1; return (Not f)
	<+> do eat LTRI; r <- p_reg; eat RTRI; f <- p_ldlTerm1; return (Unary r f)
	<+> do eat BRA; r <- p_reg; eat KET; f <- p_ldlTerm1; return (Not (Unary r (Not f)))
	<+> p_ldlPrimary

p_ldlPrimary = -- a proposition, or, recursively, a formula in parentheses.
	do
		y <- scan;
		case y of
			PROP p -> return (PropVar p)
			CONST b -> return (PropConst b)
			_ -> p_fail
	<+> do eat LPAR; f <- p_ldlFormula; eat RPAR; return f

p_reg :: Parser Token Reg_
p_reg = p_disj where
	p_disj = p_chainr (\PLUS -> Plus) (eat PLUS) p_conj
	p_conj = p_chainr (\SEMI -> Comp) (eat SEMI) p_regTerm1

p_regTerm1 =
	do r <- p_regPrimary; f r where
		f r =
			do
				eat STAR; r2 <- f r; return (Star r2)
			<+> return r

p_regPrimary = 
	do f <- p_ldlFormula; eat TEST; return (Test f)
	<+> do bf <- p_basicFormula; return (Base bf)
	<+> do eat LPAR; r <- p_reg; eat RPAR; return r

p_basicFormula :: Parser Token (BasicPropLogic Proposition)
p_basicFormula = dnf p_basicTerm1

p_basicTerm1 = 
	do eat NOT; f <- p_basicTerm1; return (Not f)
	<+>	p_basicPrimary

p_basicPrimary =
	do 
		y <- scan;
		case y of 
			PROP p -> return (PropVar p);
			CONST b -> return (PropConst b)
			_ -> p_fail
	<+> do eat LPAR; f <- p_basicFormula; eat RPAR; return f


ldlParser :: Syntax Token LDLogic_
ldlParser = (lexer, p_ldlFormula)

regParser :: Syntax Token Reg_
regParser = (lexer, p_reg)


