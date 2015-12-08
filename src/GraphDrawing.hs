-- | For drawing graphs using GraphViz's dot language
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module GraphDrawing (Drawable, DrawableWithName, Named, output, callDot) where

import Automata
import Samples
import Logic
import Data.Set.Monad as S
import Data.Map as M
import System.Process (callCommand)
import System.IO (hClose, hPutStrLn)


-- * Drawable and related classes
class Drawable a where
	draw :: a -> [String] -- ^ lines of the .gv description of the graph
	drawWithGraphName :: String -> a -> [String]
	drawWithGraphName name g = ["Digraph " ++ name ++ " {"] ++  (draw g) ++ ["}"]

data Named a = Named String a -- I will need to label nodes in the formula graph
-- to differentiate them from others.

class DrawableWithName a where
	drawWithName :: a -> String -> [String]

instance (DrawableWithName a) => Drawable (Named a) where
	draw (Named name x) = drawWithName x name

-- * Convenience functions 
label :: String -> String
label s = "[label=\"" ++ s ++ "\"]"
drawTransition :: String -> String -> String
drawTransition qStr1 qStr2 = qStr1 ++ " -> " ++ qStr2
labelTransition :: String -> String -> String -> String
labelTransition qStr1 qStr2 aStr = drawTransition qStr1 qStr2 ++ label aStr

-- | Convenience for labelling the boolean formula graph node.
boolform :: (Show q, Show a) => q -> a -> String
boolform q a = Prelude.filter (/= ' ') "boolform" ++ show q ++ show a

-- | concatenate strings with a space in between
infixr 5 +-+
(+-+) :: String -> String -> String
x +-+ y = x ++ " " ++ y


-- * Instances for Drawable


-- | Draw DFAs.
-- Prints every part of the graph specified by
-- the transition function and final state set and start state.
-- So make sure it's reachable first!
instance (Show q, Show a, Ord q, Ord a) => Drawable (DFA q a) where
	draw (FA as qs q (DTransition dt) fs) =
		["init -> " ++ show q] ++
		["init  [shape=polygon,style=bold,sides=6,color=lightblue]"] ++
		[labelTransition (show q) (show q') (show a)
			| a <- S.toList as, q <- S.toList qs, Just q' <- [M.lookup (q,a) dt]] ++
		[show q ++ "[peripheries=2, color=green]" | q <- S.toList fs]

-- | Draw AFAs.
instance (Show q, Show a, Ord q, Ord a) => Drawable (AFA q a) where
	draw (FA as qs q (ATransition at) fs) =
		["init -> " ++ show q] ++
		["init  [shape=polygon,style=bold,sides=6,color=lightblue]"] ++
		draw at ++ -- draw the transitions, which are quite involved since they have boolean formulas
		[show q ++ "[peripheries=2, color=green]" | q <- S.toList fs]



-- | Drawing boolean formulas from transition table
instance (Show q, Show a, Ord q, Ord a) => Drawable (M.Map (q, a) (BasicPropLogic q)) where
	draw at =
		concat [ case log of
			PropVar q' -> [labelTransition (show q) (show q') (show a)]
			x -> draw (Named (boolform q a) x)
			| (q,a) <- M.keys at, let log = at M.! (q,a)]

-- * Instance for DrawableWithName

-- | Lets me draw a graph of a boolean formula, named to distinguish it from other graphs
instance DrawableWithName (BasicPropLogic q) where
	-- drawWithName :: BasicPropLogic -> String -> [String]
	-- in other words, drawWithName log is a 'continuation' that 'draws'
	-- the logical formula when given the name.
	-- the arguments to the foldLog's recursive operators (arr, orr etc.)
	-- are precisely these continuations.
	drawWithName = foldLog cr pr nr arr orr ur br where
		cr b = -- PropConst
			(\name ->
				let bname = name ++ show b
				in [name +-+ label (show b)])
		pr q = -- PropVar
			error "Should not get to this case"
		nr drawCont = -- Not f
			error "Negation in what is meant to be positive boolean formula"
		arr = binopLabel "And"
		orr = binopLabel "Or"
		ur = error "No unary modalities should be here"
		br = error "No binary modalities should be here"


-- | helper for drawWithName (BasicPropLogic q)
binopLabel :: String -> (String -> [String]) -> (String -> [String]) -> (String -> [String])
binopLabel op dc1 dc2 =
	(\name ->
		let nextName1 = name ++ op ++ "1";
			nextName2 = name ++ op ++ "2"
		in [drawTransition name nextName1,
			drawTransition name nextName2,
			name +-+ label op] ++ dc1 nextName1 ++ dc2 nextName2
	)

{-

foldLog :: (Bool -> r) ->
           (p -> r) ->
           (r -> r) ->
           (r -> r -> r) -> 
           (r -> r -> r) ->
           (u -> r -> r) ->
           (b -> r -> r -> r) ->
           (PropLogic p u b) ->
           r
foldLog cr pr nr ar orr ur br log = f log where
    f (PropConst c) = cr c
    f (PropVar p) = pr p
    f (Not log) = nr $ f log
    f (And log1 log2) = ar (f log1) (f log2)
    f (Or log1 log2) = orr (f log1) (f log2)
    f (Unary u log) = ur u (f log)
    f (Binary b log1 log2) = br b (f log1) (f log2)
type BasicPropLogic p = PropLogic p Void Void


data PropLogic p u b =
  PropConst Bool |
  PropVar p |
  Not (PropLogic p u b) |
  And (PropLogic p u b) (PropLogic p u b) |
  Or (PropLogic p u b) (PropLogic p u b) |
  Unary u (PropLogic p u b) | -- unary modal operators u
  Binary b (PropLogic p u b) (PropLogic p u b) deriving (Eq, Ord) -- binary modal operators b

-}

-- * For use in the interpreter

-- | Output drawable object to file
output :: Drawable a => a -> IO ()
output g = do
	putStrLn "Name of graph:"
	name <- getLine
	let lines = drawWithGraphName name g
	appendFile "GraphDrawingOutput.gv" "\n"
	mapM_ (\l -> appendFile "GraphDrawingOutput.gv" (l++"\n")) lines

callDot :: Drawable a => a -> IO ()
callDot g =
	do
		output g
		callCommand "dot -Tpdf GraphDrawingOutput.gv -o GraphDrawingOutput.pdf"
		putStrLn "Done!"