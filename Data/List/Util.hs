module Data.List.Util where

compose :: [a -> a] -> a -> a
compose = foldr (.) id

splice :: Int -> Int -> [a] -> [a] -> [a]
splice i nDrop newPart list = take i list ++ newPart ++ drop (i + nDrop) list

insertAt :: Int -> a -> [a] -> [a]
insertAt i x = splice i 0 [x]

replicateAt :: Int -> Int -> [a] -> [a]
replicateAt i count list = splice i 1 (replicate count (list !! i)) list

duplicateAt :: Int -> [a] -> [a]
duplicateAt = flip replicateAt 2

removeAt :: Int -> [a] -> [a]
removeAt i = splice i 1 []
