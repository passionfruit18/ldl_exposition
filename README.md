# LTL and LDL for Finite Traces: Exposition
Undergraduate project for third year computer science.

[Documentation](dist/doc/html/ldl/index.html)
There is a haddock documentation in dist/doc/html/ldl/index.html.
There is also a LaTeXed project report somewhere.

I'd like to draw a graph of dependencies but that will have to wait.

Part 1:
Logic -> Automata -> Samples -> GraphDrawing
		 Automata -> GraphAlgorithms


Part 2:
Environment -> Parsing -> LDLParser -> LDLInterpreter
				 Logic -> LDLParser

Lets you enter LDL formulae in the syntax
```
ldl ::= true | false | var |
		ldl && ldl | ldl || ldl | !ldl
		<reg>ldl | [reg]ldl 
var ::= any alphanumeric string other than true, false
reg ::= bpl | reg+reg | reg;reg | reg* | ldl?
bpl ::= true | false | var |
		bpl && bpl | bpl || bpl | !bpl
```
&& binds tighter than ||.