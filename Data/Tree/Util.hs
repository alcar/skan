{-# LANGUAGE Rank2Types #-}

module Data.Tree.Util where

import Control.Applicative
import Control.Lens
import Data.List
import Data.Tree

type TreeLens a  = forall a . Lens' (Tree a) a
type TreeALens a = forall a . ALens' (Tree a) a

scanTree :: (b -> a -> b) -> b -> Tree a -> Tree b
scanTree fn init (Node element children) = Node (fn init element) $ map (scanTree fn (fn init element)) children

mapAccumTree :: (b -> a -> (b,c)) -> b -> Tree a -> Tree c
mapAccumTree fn state (Node element children) = Node nextElement $ map (mapAccumTree fn nextState) children 
    where (nextState, nextElement) = fn state element

zipWithTree :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
zipWithTree f (Node x xs) (Node y ys) = Node (f x y) (zipWith (zipWithTree f) xs ys)

paths :: Tree a -> [[a]]
paths (Node element []) = [[element]]
paths (Node element children) = map (element :) $ concat $ map paths children

getPath :: [Int] -> Tree a -> a
getPath path tree = rootLabel $ foldl' (\ (Node _ children) pos -> children !! pos) tree path

pretty :: (Show a) => Tree a -> String
pretty = drawTree . fmap show

prettyPrint :: (Show a) => Tree a -> IO ()
prettyPrint = putStrLn . pretty

-- Infinite tagged tree such that each node holds its path. 
-- That is, `infinitePathTree ^? ix path == Just path`.
infinitePathTree :: Tree [Int]
infinitePathTree = go [] where
    go p = Node p (map (\ i -> go (p++[i])) [0..])

-- There is currently a bug on Lens's Ixed instance for trees
-- that causes it to malfunction. That fixes the bug. Should
-- be removed when Lens accepts the patch (probably soon).
ixTree xs0 f = go xs0 where
    go [] (Node a as) = f a <&> \a' -> Node a' as
    go (i:is) t@(Node a as) | i < 0     = pure t
                            | otherwise = Node a <$> goto is as i
    goto is (a:as) 0 = go is a <&> (:as)
    goto is (a:as) n = (goto is as $! n - 1) <&> (a:)
    goto _  []     _ = pure []

-- atPath n = lens ((!! n) . subForest) (\ k (Node x xs) -> Node x (xs & ix n .~ k))
-- type Index (Tree a) = Int
-- a = (Node 1 [Node 2 [Node 3 [], Node 4 []], Node 5 []])

-- main = do
    -- putStrLn $ drawTree $ fmap show a
    -- print $ (a ^? (ix [0,0,0]))
    -- print $ [1,2,3] ^? ix 0

