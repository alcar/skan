{-# LANGUAGE FlexibleContexts #-}

-- An n-dimensional grid with a more convenient API than Array/REPA.
-- It is just a thin, almost 1 to 1 wrapper around the Vector module.

module Data.Grid where

import Data.GridPos
import Prelude hiding (length,map,zipWith,sum)
import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as UV

data GGrid sh v a = Grid {
    shape  :: sh,
    buffer :: v a
    } deriving Show

type Grid sh a = GGrid sh V.Vector a
type UGrid sh a = GGrid sh UV.Vector a

fromList :: (GV.Vector v a) => s -> [a] -> GGrid s v a
fromList shape list = Grid shape (GV.fromList list)

grid :: s -> [a] -> Grid s a
grid = fromList

ugrid :: (GV.Vector UV.Vector a) => s -> [a] -> UGrid s a
ugrid = fromList

length :: (GV.Vector v a) => GGrid s v a -> Int
length = GV.length . buffer

null :: (GV.Vector v a) => GGrid s v a -> Bool
null = GV.null . buffer

(!) :: (GV.Vector v a, GridPos s) => GGrid (s Int) v a -> s Int -> a
(Grid shape buffer) ! pos = buffer GV.! posToIndex shape pos

(!?) :: (GV.Vector v a, GridPos s) => GGrid (s Int) v a -> (s Int) -> Maybe a
(Grid shape buffer) !? pos = buffer GV.!? posToIndex shape pos

generate :: (F.Foldable s, GridPos s, GV.Vector v a) => s Int -> (s Int -> a) -> GGrid (s Int) v a
generate shape fn = Grid shape (GV.generate (F.foldr' (*) 1 shape) (fn . indexToPos shape))

replicate :: (F.Foldable s, GridPos s, GV.Vector v a) => s Int -> a -> GGrid (s Int) v a
replicate shape val = generate shape (const val)

(//) :: (GridPos s, GV.Vector v a) => GGrid (s Int) v a -> [(s Int,a)] -> GGrid (s Int) v a
(//) (Grid shape buffer) deltas = Grid shape (buffer GV.// (fmap (\ (pos,x) -> (posToIndex shape pos, x)) deltas))

update :: (GridPos s, GV.Vector v a, GV.Vector v (Int, a), GV.Vector v (s Int,a))
       => GGrid (s Int) v a -> v (s Int,a) -> GGrid (s Int) v a
update (Grid shape buffer) deltas = Grid shape (GV.update buffer (GV.map (\ (pos,x) -> (posToIndex shape pos, x)) deltas))

unsafeUpd :: (GridPos s, GV.Vector v a) => GGrid (s Int) v a -> [(s Int,a)] -> GGrid (s Int) v a
unsafeUpd (Grid shape buffer) deltas = Grid shape (GV.unsafeUpd buffer (fmap (\ (pos,x) -> (posToIndex shape pos, x)) deltas))

updateUpdate :: (GridPos s, GV.Vector v a, GV.Vector v (Int, a), GV.Vector v (s Int,a))
             => GGrid (s Int) v a -> v (s Int,a) -> GGrid (s Int) v a
updateUpdate (Grid shape buffer) deltas = Grid shape (GV.unsafeUpdate buffer (GV.map (\ (pos,x) -> (posToIndex shape pos, x)) deltas))

map :: (GV.Vector v a, GV.Vector v b) => (a -> b) -> GGrid s v a -> GGrid s v b
map f (Grid shape buffer) = Grid shape (GV.map f buffer)

imap :: (GridPos s, GV.Vector v a, GV.Vector v b) => (s Int -> a -> b) -> GGrid (s Int) v a -> GGrid (s Int) v b
imap f (Grid shape buffer) = Grid shape (GV.imap (f . indexToPos shape) buffer)

zipWith :: (GV.Vector v a, GV.Vector v b, GV.Vector v c) => (a -> b -> c) -> GGrid s v a -> GGrid s v b -> GGrid s v c
zipWith f (Grid sa ba) (Grid _ bb) = Grid sa (GV.zipWith f ba bb)

izipWith :: (GridPos s, GV.Vector v a, GV.Vector v b, GV.Vector v c)
         => (s Int -> a -> b -> c) -> GGrid (s Int) v a -> GGrid (s Int) v b -> GGrid (s Int) v c
izipWith f (Grid sa ba) (Grid _ bb) = Grid sa (GV.izipWith (\ i x y -> f (indexToPos sa i) x y) ba bb)

zip :: (GV.Vector v a, GV.Vector v b, GV.Vector v (a,b)) => GGrid s v a -> GGrid s v b -> GGrid s v (a, b)
zip = zipWith (,)

foldl :: (GV.Vector v b) => (a -> b -> a) -> a -> GGrid s v b -> a
foldl f i = GV.foldl f i . buffer

foldl1 :: (GV.Vector v a) => (a -> a -> a) -> GGrid s v a -> a
foldl1 f = GV.foldl1 f . buffer

foldr :: (GV.Vector v a) => (a -> b -> b) -> b -> GGrid s v a -> b
foldr f i = GV.foldr f i . buffer

foldr1 :: (GV.Vector v a) => (a -> a -> a) -> GGrid s v a -> a
foldr1 f = GV.foldr1 f . buffer

foldl' :: (GV.Vector v b) => (a -> b -> a) -> a -> GGrid s v b -> a
foldl' f i = GV.foldl' f i . buffer

foldl1' :: (GV.Vector v a) => (a -> a -> a) -> GGrid s v a -> a
foldl1' f = GV.foldl1' f . buffer

foldr' :: (GV.Vector v a) => (a -> b -> b) -> b -> GGrid s v a -> b
foldr' f i = GV.foldr' f i . buffer

foldr1' :: (GV.Vector v a) => (a -> a -> a) -> GGrid s v a -> a
foldr1' f = GV.foldr1' f . buffer

sum :: (GV.Vector v a, Num a) => GGrid s v a -> a
sum = GV.sum . buffer

product :: (GV.Vector v a, Num a) => GGrid s v a -> a
product = GV.product . buffer
