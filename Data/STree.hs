import Util
import Control.Monad.Identity

data Tree a = Node (Tree a) (Tree a) | Leaf a | Nil
    deriving Show

insert :: (Eq k, Num k) => [k] -> v -> Tree ([k],v) -> Tree ([k],v)
insert key val node = go 0 key node where
    go d (x:xs) Nil = Leaf (key,val)
    go d (x:xs) (Leaf (kb,vb)) = undraw (key,val) (kb,vb) (x:xs) (drop d kb)
    go d (x:xs) (Node a b)
        | x == 0    = Node (go (d+1) xs a) b
        | otherwise = Node a (go (d+1) xs b)

undraw :: (Num a, Num b, Eq a, Eq b) => (k, v) -> (k, v) -> [a] -> [b] -> Tree (k, v)
undraw (aKey,aVal) (bKey,bVal) = go where
    go (ka:kas) (kb:kbs)
        | ka == 0 && kb == 0 = Node (go kas kbs) Nil
        | ka == 1 && kb == 0 = Node (Leaf (bKey,bVal)) (Leaf (aKey,aVal))
        | ka == 0 && kb == 1 = Node (Leaf (aKey,aVal)) (Leaf (bKey,bVal))
        | ka == 1 && kb == 1 = Node Nil (go kas kbs)

main = do
    let tree = comb [
            insert [0,0,0,0] "foo",
            insert [0,0,0,1] "bar",
            insert [1,1,1] "aff"
            ] Nil
    print $ (tree :: Tree ([Int],String))
