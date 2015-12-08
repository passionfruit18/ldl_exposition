# ldl_new_development

Undergraduate project for third year computer science.

[Documentation](dist/doc/html/ldl/index.html)


Automata, Logic, Samples, GraphDrawing, GraphAlgorithms


Part 2:
Environment -> Parsing -> LDLParser -> LDLInterpreter

Lets you enter LDL formulae in the syntax
```
ldl ::= true | false | var |
		ldl && ldl | ldl || ldl | !ldl
		\<reg\>ldl | [reg]ldl 
var ::= any alphanumeric string other than true, false
reg ::= bpl | reg+reg | reg;reg | reg* | 
```
&& binds tighter than ||.