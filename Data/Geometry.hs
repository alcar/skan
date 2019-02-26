{-# LANGUAGE
    DeriveFunctor,
    MultiParamTypeClasses,
    TemplateHaskell,
    TypeFamilies,
    FlexibleInstances,
    RankNTypes,
    FlexibleContexts,
    TupleSections,
    DeriveGeneric
    #-}

module Data.Geometry where

import Control.Monad
import Data.Foldable
import Data.Traversable
import Linear hiding (trace)
import Prelude hiding (foldr, foldr1, and, all, sum, minimum)

type Quad v a = V4 (v a)

type Surface v a = a -> a -> v a

type Mesh v a = [Quad v a]

data AABB v a = AABB {
    aabbMin :: !(v a),
    aabbMax :: !(v a)
    } deriving (Show)

data Ray v a = Ray {
    rayPos :: !(v a),
    rayDir :: !(v a)
    } deriving (Show)

data Plane v a = Plane {
    planePos :: !(v a),
    planeNorm :: !(v a)
    } deriving (Show)

data Sphere v a = Sphere {
    spherePos :: !(v a),
    sphereRad :: !a
    } deriving (Show)

-- A pivot point is a triple (position, direction, normal)
type Pivot a = V3 (V3 a)

type Projection a = V3 a -> V3 a

projectOnPlane :: (Metric f, Num (f a), Num a) => Plane f a -> f a -> f a
projectOnPlane (Plane pPos pNorm) pos = pos - pNorm ^* dot (pos - pPos) pNorm

surfaceToMesh :: (Enum a, Fractional a) => a -> a -> Surface f a -> Mesh f a
surfaceToMesh p q surface = [makeQuad u v | v <- [0..p-1], u <- [0..q-1]] where
    makeQuad u v = V4
        (surface (u/q)     (v/p))
        (surface ((u+1)/q) (v/p))
        (surface ((u+1)/q) ((v+1)/p))
        (surface (u/q)     ((v+1)/p))

hitPlane :: (Num (f a), Metric f, Num a, Fractional a) => Ray f a -> Plane f a -> f a
hitPlane (Ray rPos rDir) (Plane pPos pNorm) = rPos + dot (pPos - rPos) pNorm / dot rDir pNorm *^ rDir

hitQuad :: (Ord a, Fractional a) => Ray V3 a -> Quad V3 a -> Maybe (V3 a)
hitQuad ray (quad@(V4 a b c d)) = if hitInsideQuad then Just hitPoint else Nothing where
    hitInsideQuad = insideQuad hitPoint quad
    planeNormal   = cross (b - a) (d - a)
    hitPoint      = hitPlane ray (Plane a planeNormal)

crossSphere :: (Metric f, Ord a, Num (f a), Floating a) => Ray f a -> Sphere f a -> [a]
crossSphere (Ray rPos rDir) (Sphere sPos sRad) = if d > 0 then ts else [] where
    a  = quadrance rDir
    b  = dot (2 *^ rDir) (rPos - sPos)
    c  = quadrance (rPos - sPos) - sRad ** 2
    d  = b ** 2 - 4 * a * c
    ts = [(-b+sqrt d)/(2*a), (-b-sqrt d)/(2*a)]

hitSphere :: (Metric f, Ord a, Num (f a), Floating a) => Ray f a -> Sphere f a -> Maybe (f a)
hitSphere ray@(Ray rPos rDir) sphere = hit where
    hit  = if not (null hits) then Just (rPos + (minimum hits *^ rDir)) else Nothing
    hits = filter (> 0) $ crossSphere ray sphere

hitSpheres :: (Metric f, Ord a, Num (f a), Floating a) => Ray f a -> [Sphere f a] -> Maybe (f a)
hitSpheres ray = foldr mplus mzero . map (hitSphere ray)

crossAABB :: (Foldable f, Metric f, Ord a, Num (f a), Fractional (f a), Floating a) => Ray f a -> AABB f a -> [a]
crossAABB (Ray rPos rDir) (AABB aabbMin aabbMax) = [tmin, tmax] where
    t1   = (aabbMin - rPos)/rDir
    t2   = (aabbMax - rPos)/rDir
    tmin = foldr1 max $ liftI2 min t1 t2
    tmax = foldr1 min $ liftI2 max t1 t2

hitAABB :: (Foldable f, Metric f, Ord a, Num (f a), Fractional (f a), Floating a) => Ray f a -> AABB f a -> Maybe (f a)
hitAABB ray@(Ray rPos rDir) aabb
    | tmin >= 0 && tmin < tmax = Just $ rPos + tmin *^ rDir
    | otherwise                = Nothing
    where [tmin,tmax] = crossAABB ray aabb

insideAABB :: (Foldable f, Additive f, Ord a) => f a -> AABB f a -> Bool
insideAABB pos (AABB from to) = and (liftI2 (<=) from pos) && and (liftI2 (<=) pos to)

insideQuad :: (Metric f, Ord a, Num (f a), Num a) => f a -> V4 (f a) -> Bool
insideQuad pos (V4 a b c d) = all inside borders where
    borders      = [(a,b),(b,c),(c,d),(d,a)]
    inside (a,b) = dot (b - a) (pos - a) > 0

interpolateList :: (RealFrac s, Fractional a) => [a] -> s -> a
interpolateList vals pos = valA * (1 - pos') + valB * pos' where
    floatIndex = fromIntegral (length vals - 1) * pos
    index      = min (floor floatIndex) (length vals - 2)
    pos'       = realToFrac $ floatIndex - fromIntegral index
    valA       = vals !! index
    valB       = vals !! (index + 1)
