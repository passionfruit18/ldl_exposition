-- | For drawing graphs using GraphViz's dot language
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module GraphDrawing (Drawable, output, callDot) where

import Automata
import Samples
import Logic
import Data.Set.Monad as S
import Data.Map as M
import System.Process (callCommand)
import System.IO (hClose, hPutStrLn)

type Name = String
type GraphInstrs = [String]

-- * Drawable and related classes
class Drawable a where
	draw :: a -> GraphInstrs -- ^ lines of the .gv description of the graph
	drawWithGraphName :: Name -> a -> GraphInstrs
	drawWithGraphName name g = ["Digraph " ++ name ++ " {"] ++  (draw g) ++ ["}"]


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

initString = "init  [shape=polygon,style=bold,sides=6,color=lightblue]"
finalString = "[peripheries=2, color=green]"
preamble qs q = [show q +-+ label (labelState q) | q <- S.toList qs] ++
				["init ->" +-+ show q, initString]
postamble fs = [show q +-+ finalString | q <- S.toList fs]

-- | Draw DFAs.
-- Prints every part of the graph specified by
-- the transition function and final state set and start state.
-- So make sure it's reachable first!
instance (Show q, LabelState q, Show a, Ord q, Ord a) => Drawable (DFA q a) where
	draw (FA as qs q (DTransition dt) fs) =
		preamble qs q ++
		[labelTransition (show q) (show q') (show a)
			| a <- S.toList as, q <- S.toList qs, Just q' <- [M.lookup (q,a) dt]] ++
		postamble fs

-- | Draw NFAs.
instance (Show q, LabelState q, Show a, Ord q, Ord a) => Drawable (NFA q a) where
	draw (FA as qs q (NTransition nt) fs) =
		preamble qs q ++
		[labelTransition (show q)  (show q') (show a) 
			| a <- S.toList as, q <- S.toList qs, q' <- S.toList (setLookup (q,a) nt)] ++
		postamble fs


-- | Draw AFAs.
instance (Show q, Show a, Ord q, Ord a, LabelState q) => Drawable (AFA q a) where

	draw (FA as qs q (ATransition at) fs) =
		preamble qs q ++
		draw at ++ -- draw the transitions, which are quite involved since they have boolean formulas
		postamble fs

instance (Show q, Show a, LabelState q, Ord q, Ord a) => Drawable (M.Map (q,a) (BasicPropLogic q)) where
	draw at =
		concat [draw (WithInfo formula (Info (show q) (Just (show a)) (Just (Initial ("On" ++ show a)))))
				| (q,a) <- M.keys at, let formula = at M.! (q,a)]



class DrawableWithInfo a where
	drawWithInfo :: a -> Info -> GraphInstrs

data WithInfo a = WithInfo a Info

instance (DrawableWithInfo a) => Drawable (WithInfo a) where
	draw (WithInfo x info) = drawWithInfo x info

-- | could carry a label,
data Info = Info Name (Maybe Label2) (Maybe Diff)
type Label2 = String
data Diff = LeftBranch | RightBranch | Initial String

instance Show q => DrawableWithInfo (BasicPropLogic q) where
	-- the return type of the foldLog is something that takes
	-- an "Info" and returns GraphInstrs.
	drawWithInfo = foldLog cr pr nr arr orr ur br where
		cr b = -- PropConst
			(\(Info name ml mdiff) ->
				let boolName = newName mdiff (name ++ show b)
				in [drawTransML ml name boolName, boolName +-+ label (show b)])
		pr q = -- PropVar
			(\(Info name ml mdiff) ->
				[drawTransML ml name (show q)])
		nr drawCont = -- Not f
			error "Negation in what is meant to be a positive boolean formula"
		arr = binopLabelInfo "And"
		orr = binopLabelInfo "Or"
		ur = error "No unary modalities should be here"
		br = error "No binary modalities should be here"
newName :: (Maybe Diff) -> Name -> Name
newName mdiff baseName =
	case mdiff of
		Nothing -> baseName
		Just LeftBranch -> baseName ++ "Left"
		Just RightBranch -> baseName ++ "Right"
		Just (Initial x) -> baseName ++ x
drawTransML :: (Maybe Label2) -> Name -> Name -> String
drawTransML ml name1 name2 =
	case ml of
		Just l -> labelTransition name1 name2 l
		Nothing -> drawTransition name1 name2
-- | helper for drawWithInfo (BasicPropLogic q)
binopLabelInfo :: String -> (Info -> GraphInstrs) -> (Info -> GraphInstrs) -> (Info -> GraphInstrs)
binopLabelInfo op dc1 dc2 =
	(\(Info name ml mdiff) ->
		let myName = newName mdiff (name ++ op)
		in [drawTransML ml name myName,
			myName +-+ label op] ++
			dc1 (Info myName Nothing (Just LeftBranch)) ++
			dc2 (Info myName Nothing (Just RightBranch))
	)


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