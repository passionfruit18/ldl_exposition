-- | For drawing graphs using GraphViz's dot language
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module GraphDrawingIO (DrawableIO, DrawableWithNameIO, NamedIO, outputIO, callDotIO) where

import Automata
import Samples
import Logic
import Data.Set.Monad as S
import Data.Map as M
import System.Process (callCommand)
import System.IO (hClose, hPutStrLn)
import Control.Monad


-- * Drawable and related classes
class DrawableIO a where
	drawIO :: a -> IO [String] -- ^ lines of the .gv description of the graph
	drawWithGraphNameIO :: String -> a -> IO [String]
	drawWithGraphNameIO name g = do
		d <- drawIO g
		return (["Digraph " ++ name ++ " {"] ++ d ++ ["}"])

data NamedIO a = NamedIO String a -- I will need to label nodes in the formula graph
-- to differentiate them from others.

class DrawableWithNameIO a where
	drawWithNameIO :: a -> String -> IO [String]

instance (DrawableWithNameIO a) => DrawableIO (NamedIO a) where
	drawIO (NamedIO name x) = drawWithNameIO x name

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
instance (Show q, Show a, Ord q, Ord a) => DrawableIO (DFA q a) where
	drawIO (FA as qs q (DTransition dt) fs) =
		return (
		["init -> " ++ show q] ++
		["init  [shape=polygon,style=bold,sides=6,color=lightblue]"] ++
		[labelTransition (show q) (show q') (show a)
			| a <- S.toList as, q <- S.toList qs, Just q' <- [M.lookup (q,a) dt]] ++
		[show q ++ "[peripheries=2, color=green]" | q <- S.toList fs])

-- | Draw AFAs.
instance (Show q, Show a, Ord q, Ord a) => DrawableIO (AFA q a) where
	drawIO (FA as qs q (ATransition at) fs) = 
		do
			d <- drawIO at
			return(["init -> " ++ show q] ++
					["init  [shape=polygon,style=bold,sides=6,color=lightblue]"] ++
					d ++ -- draw the transitions, which are quite involved since they have boolean formulas
					[show q ++ "[peripheries=2, color=green]" | q <- S.toList fs])



-- | Drawing boolean formulas from transition table
instance (Show q, Show a, Ord q, Ord a) => DrawableIO (M.Map (q, a) (BasicPropLogic q)) where
	drawIO at = liftM concat $
		sequence [ case log of
			PropVar q' ->
				(do putStrLn (show log); return [labelTransition (show q) (show q') (show a)])
			PropConst b -> let bname = name ++ show b
							in return [drawTransition (show q) bname,
								bname +-+ label (show b)]	
			log -> (do
				d <- drawIO (NamedIO name log)
				return (d ++ [labelTransition (show q) name (show a)]))
			| (q,a) <- M.keys at, let log = at M.! (q,a), let name = boolform q a]

-- * Instance for DrawableWithName

-- | Lets me draw a graph of a boolean formula, named to distinguish it from other graphs
instance Show q => DrawableWithNameIO (BasicPropLogic q) where
	-- drawWithName :: BasicPropLogic -> String -> IO [String]
	-- in other words, drawWithName log is a 'continuation' that 'draws'
	-- the logical formula when given the name.
	-- the arguments to the foldLog's recursive operators (arr, orr etc.)
	-- are precisely these continuations.
	drawWithNameIO = foldLog cr pr nr arr orr ur br where
		cr b = -- PropConst
			(\name ->
				let bname = name ++ show b
				in return [name +-+ label (show b)])
		pr q = -- PropVar
			(\name ->
				return [name +-+ label (show q)]-- ok, so there's a problem here, in that
				-- we will create separate nodes labelled with the same state, with different
				-- names.
				)
		nr drawCont = -- Not f
			error "Negation in what is meant to be positive boolean formula"
		arr = binopLabel "And"
		orr = binopLabel "Or"
		ur = error "No unary modalities should be here"
		br = error "No binary modalities should be here"


-- | helper for drawWithName (BasicPropLogic q)
binopLabel :: String -> (String -> IO [String]) -> (String -> IO [String]) -> (String -> IO [String])
binopLabel op dc1 dc2 =
	(\name ->
		do
			putStrLn (op +-+ name)
			let nextName1 = name ++ op ++ "1"
			let	nextName2 = name ++ op ++ "2"
			d1 <- dc1 nextName1
			d2 <- dc2 nextName2
			return ([drawTransition name nextName1,
				drawTransition name nextName2,
				name +-+ label op] ++ d1 ++ d2)
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
outputIO :: DrawableIO a => a -> IO ()
outputIO g = do
	putStrLn "Name of graph:"
	name <- getLine
	lines <- drawWithGraphNameIO name g
	appendFile "GraphDrawingOutput.gv" "\n"
	mapM_ (\l -> appendFile "GraphDrawingOutput.gv" (l++"\n")) lines

callDotIO :: DrawableIO a => a -> IO ()
callDotIO g =
	do
		outputIO g
		callCommand "dot -Tpdf GraphDrawingOutput.gv -o GraphDrawingOutput.pdf"
		putStrLn "Done!"