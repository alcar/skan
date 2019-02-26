{-# LANGUAGE RankNTypes #-}

module Data.Skeleton.Gloss where

import Control.Lens
import Data.Geometry
import Data.Skeleton
import Data.Tree.Util
import Graphics.Gloss
import Data.Tuple
import Linear hiding (trace)

toGlossPoint :: V3 Float -> Point
toGlossPoint (V3 x y _) = (x,y)

renderSkeleton
    :: Projection Float
    -> V3 Float
    -> V3 Float
    -> V3 Float
    -> [Int]
    -> Skeleton Float
    -> Picture
renderSkeleton projection pos dir norm focus skeleton = pictures [
    renderSkeletonLines projection pos dir skeleton,
    renderSkeletonNormals projection pos dir norm skeleton,
    renderSkeletonFocusedJoint projection pos dir focus skeleton]

-- Renders the skeleton by tracing a line through its bones.
renderSkeletonLines
    :: Projection Float
    -> V3 Float
    -> V3 Float
    -> Skeleton Float
    -> Picture
renderSkeletonLines projection pos dir skeleton
    = pictures
    . map line
    . paths
    . fmap toGlossPoint
    . fmap projection
    $ jointPositions pos dir skeleton

-- Render the normals of each joint of the skeleton.
renderSkeletonNormals
    :: Projection Float
    -> V3 Float
    -> V3 Float
    -> V3 Float
    -> Skeleton Float
    -> Picture
renderSkeletonNormals project pos dir norm skeleton
    = pictures
    . concat
    . map (map line)
    . paths
    . fmap (map toGlossPoint)
    . fmap (map project)
    $ zipWithTree (\ pos norm -> [pos, pos^+^norm*5])
        (jointPositions pos dir skeleton)
        (jointDirections norm skeleton)

renderSkeletonFocusedJoint
    :: Projection Float
    -> V3 Float
    -> V3 Float
    -> [Int]
    -> Skeleton Float
    -> Picture
renderSkeletonFocusedJoint project pos dir focus skeleton
    = uncurry
        translate (toGlossPoint (project ((jointPositions pos dir skeleton) ^?! ix focus)))
        (circle 3)
