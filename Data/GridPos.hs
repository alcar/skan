module Data.GridPos where

import Linear

-- A GridPos is any container that can be used as the index
-- of an n-dimensional grid represented by a flat buffer.
class GridPos f where
    -- Maps an n-dimensional position to an integral index.
    posToIndex :: (Integral a) => f a -> f a -> a
    -- Maps the integral index to a n-dimensional position.
    indexToPos :: (Integral a) => f a -> a -> f a

instance GridPos [] where
    posToIndex shape pos = sum (zipWith (*) pos (scanl (*) 1 shape))
    indexToPos shape idx = zipWith mod (scanl div idx shape) shape

instance GridPos V2 where
    posToIndex (V2 w h) (V2 x y) = x + w * y
    indexToPos (V2 w h) i = V2 (mod i w) (mod (div i w) h)

instance GridPos V3 where
    posToIndex (V3 w h d) (V3 x y z) = x + w * y + w * h * z
    indexToPos (V3 w h d) i = V3 (mod i w) (mod (div i w) h) (mod (div (div i w) h) d)
