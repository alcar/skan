{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Data.Skeleton where

-- http://www.scielo.org.za/img/revistas/sajs/v104n3-4/a10tab02.gif

import Control.Applicative
import Control.Lens
import Data.Geometry
import Data.Tree
import Data.Tree.Util
import Linear
import Numeric

-- A bone is just a length and a rotation.
data Bone a = Bone {_boneLen :: a, _boneRot :: Quaternion a} deriving (Read, Show)
makeLenses ''Bone

-- A skeleton consists of a labelled tree of bones.
type Skeleton a = Tree (Bone a)
type Pose a     = Tree (Quaternion a)

identityQuaternion :: (Fractional a, Conjugate a) => Quaternion a
identityQuaternion = Quaternion 1.0 (V3 0.0 0.0 0.0)

jointGlobalRotations :: (Conjugate a, Epsilon a, RealFloat a) => Skeleton a -> Tree (Quaternion a)
jointGlobalRotations = scanTree (*) identityQuaternion . fmap _boneRot

jointDirections :: (Conjugate a, RealFloat a, Epsilon a) => V3 a -> Skeleton a -> Tree (V3 a)
jointDirections dir = fmap (flip rotate dir) . jointGlobalRotations

jointPositions :: (Num a, Conjugate a, Epsilon a, RealFloat a) => V3 a -> V3 a -> Skeleton a -> Tree (V3 a)
jointPositions pos dir skeleton
    = scanTree (+) pos
    $ zipWithTree (^*)
        (jointDirections dir skeleton)
        (fmap _boneLen skeleton)

-- A cursor for easier skeletal navigation.
data SkeletonCursor = SkeletonCursor {
    _focus  :: [Int]            ,
    _up     :: SkeletonCursor   ,
    _left   :: SkeletonCursor   ,
    _down   :: SkeletonCursor   ,
    _right  :: SkeletonCursor   } deriving (Read, Show)
makeLenses ''SkeletonCursor



-- Given an initial pivot point and a skeleton, builds
-- the corresponding tree of pivot points.
-- skeletonToPivotTree :: (Conjugate a, Epsilon a, RealFloat a) => Pivot a -> Skeleton a -> PivotTree a
-- skeletonToPivotTree (V3 pos dir norm) skeleton = mapAccumTree go (identityQuaternion, pos) skeleton where
    -- go (rot, pos) bone = ((newRot, newPos), pivot) where
        -- newRot = rot * _boneRot bone
        -- newPos = pos + rotate newRot dir ^* _boneLen bone
        -- pivot  = V3 newPos (rotate newRot dir) (rotate newRot norm)

-- Convenient function for bone creation.
-- bone :: a -> Quaternion a -> Forest (Bone a) -> Tree (Bone a)
-- bone len rot children = Node (Bone len rot) children

-- applyBone :: (RealFloat a, Conjugate a) => (Pivot a) -> Bone a -> (Pivot a)
-- applyBone (V3 pos dir norm) bone = V3 pos' dir' norm' where
    -- pos'  = pos + fmap (* (_boneLen bone)) dir'
    -- dir'  = rotate (_boneRot bone) dir
    -- norm' = rotate (_boneRot bone) norm
