-- | For drawing graphs using GraphViz's dot language
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module GraphDrawing where

import Automata
import Samples
import Data.Set.Monad as S
import Data.Map as M
import System.Process (callCommand)
import System.IO (hClose, hPutStrLn)
import Data.Text.IO (hGetContents)

class Drawable a where
	draw :: a -> [String] -- ^ lines of the .gv description of the graph
	drawWithName :: String -> a -> [String]
	drawWithName name g = ["Digraph " ++ name ++ " {"] ++  (draw g) ++ ["}"]

instance (Show q, Show a, Show r) => Drawable (M.Map (q, a) r) where
	draw g = [""]

-- | Prints every part of the graph specified by
-- the transition function and final state set and start state.
-- So make sure it's reachable first!
instance (Show q, Show a, Ord q, Ord a) => Drawable (DFA q a) where
	draw (FA as qs q (DTransition dt) fs) =
		["init -> " ++ show q] ++
		["init  [shape=polygon,style=bold,sides=6,color=lightblue]"] ++
		[show q ++
		" -> " ++
		show q' ++
			"[label=\"" ++ show a ++ "\"]"
			| a <- S.toList as, q <- S.toList qs, Just q' <- [M.lookup (q,a) dt]] ++
		[show q ++ "[peripheries=2, color=green]" | q <- S.toList fs]

-- | Output drawable object to file
output :: Drawable a => a -> IO ()
output g = do
	putStrLn "Name of graph:"
	name <- getLine
	let lines = drawWithName name g
	appendFile "GraphDrawingOutput.gv" "\n"
	mapM_ (\l -> appendFile "GraphDrawingOutput.gv" (l++"\n")) lines

callDot :: Drawable a => a -> IO ()
callDot g =
	do
		output g
		callCommand "dot -Tpdf GraphDrawingOutput.gv -o GraphDrawingOutput.pdf"
		putStrLn "Done!"