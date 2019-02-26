{-# LANGUAGE RankNTypes, TemplateHaskell, ImpredicativeTypes, NoMonomorphismRestriction #-}

module Data.Skeleton.Human where

import Control.Applicative
import Control.Lens
import Data.Skeleton
import Data.Tree
import Data.Tree.Util
import Linear

-- Helpers (todo: move this to a better place)
roll, pitch, yaw :: (Epsilon a, RealFloat a, Conjugate a) => a -> Quaternion a
roll    = axisAngle (V3 1 0 0)
pitch   = axisAngle (V3 0 1 0)
yaw     = axisAngle (V3 0 0 1)

-- Arrays.
root            = []                :: [Int]
lowerSpine      = [0]               :: [Int]
middleSpine     = [0,0]             :: [Int]
upperSpine      = [0,0,0]           :: [Int]
neck            = [0,0,0,0]         :: [Int]
leftClavicle    = [0,0,0,1]         :: [Int]
leftShoulder    = [0,0,0,1,0]       :: [Int]
leftForearm     = [0,0,0,1,0,0]     :: [Int]
leftHand        = [0,0,0,1,0,0,0]   :: [Int]
rightClavicle   = [0,0,0,2]         :: [Int]
rightShoulder   = [0,0,0,2,0]       :: [Int]
rightForearm    = [0,0,0,2,0,0]     :: [Int]
rightHand       = [0,0,0,2,0,0,0]   :: [Int]
leftHip         = [1]               :: [Int]
leftThigh       = [1,0]             :: [Int]
leftShin        = [1,0,0]           :: [Int]
leftFoot        = [1,0,0,0]         :: [Int]
rightHip        = [2]               :: [Int]
rightThigh      = [2,0]             :: [Int]
rightShin       = [2,0,0]           :: [Int]
rightFoot       = [2,0,0,0]         :: [Int]

-- Cursor for intuitive skeletal navigation.
humanSkeletonCursor :: SkeletonCursor
humanSkeletonCursor = root' where
    root'          = SkeletonCursor root lowerSpine' leftHip' root' rightHip'
    lowerSpine'    = SkeletonCursor lowerSpine middleSpine' lowerSpine' root' lowerSpine'
    middleSpine'   = SkeletonCursor middleSpine upperSpine' middleSpine' lowerSpine' middleSpine'
    upperSpine'    = SkeletonCursor upperSpine neck' leftClavicle' middleSpine' rightClavicle'
    neck'          = SkeletonCursor neck neck' neck' upperSpine' neck'
    leftHip'       = SkeletonCursor leftHip root' leftThigh' leftThigh' root'
    leftThigh'     = SkeletonCursor leftThigh leftHip' leftShin' leftShin' leftHip'
    leftShin'      = SkeletonCursor leftShin leftThigh' leftFoot' leftFoot' leftThigh'
    leftFoot'      = SkeletonCursor leftFoot leftShin' leftFoot' leftFoot' leftShin'
    rightHip'      = SkeletonCursor rightHip root' root' rightThigh' rightThigh'
    rightThigh'    = SkeletonCursor rightThigh rightHip' rightHip' rightShin' rightShin'
    rightShin'     = SkeletonCursor rightShin rightThigh' rightThigh' rightFoot' rightFoot'
    rightFoot'     = SkeletonCursor rightFoot rightShin' rightShin' rightFoot' rightFoot'
    leftClavicle'  = SkeletonCursor leftClavicle upperSpine' leftShoulder' leftShoulder' upperSpine'
    leftShoulder'  = SkeletonCursor leftShoulder leftClavicle' leftForearm' leftForearm' leftClavicle'
    leftForearm'   = SkeletonCursor leftForearm leftShoulder' leftHand' leftHand' leftShoulder'
    leftHand'      = SkeletonCursor leftHand leftForearm' leftHand' leftHand' leftForearm'
    rightClavicle' = SkeletonCursor rightClavicle upperSpine' upperSpine' rightShoulder' rightShoulder'
    rightShoulder' = SkeletonCursor rightShoulder rightClavicle' rightClavicle' rightForearm' rightForearm'
    rightForearm'  = SkeletonCursor rightForearm rightShoulder' rightShoulder' rightHand' rightHand'
    rightHand'     = SkeletonCursor rightHand rightForearm' rightForearm' rightHand' rightHand'

-- A default human skeleton/pose.
humanSkeleton :: (Epsilon a, RealFloat a, Conjugate a) => Skeleton a
humanSkeleton = root where
    root          = Node (Bone 0    (roll 0))           [lowerSpine, leftHip, rightHip]
    lowerSpine    = Node (Bone 16   (roll 0))           [middleSpine]
    middleSpine   = Node (Bone 16   (roll 0))           [upperSpine]
    upperSpine    = Node (Bone 16   (roll 0))           [neck, leftClavicle, rightClavicle]
    neck          = Node (Bone 12   (roll 0))           []
    leftClavicle  = Node (Bone 22   (roll (pi*7/12)))   [leftShoulder]
    rightClavicle = Node (Bone 22   (roll (-pi*7/12)))  [rightShoulder]
    leftShoulder  = Node (Bone 35   (roll (pi*5/12)))   [leftForearm]
    rightShoulder = Node (Bone 35   (roll (-pi*5/12)))  [rightForearm]
    leftForearm   = Node (Bone 28   (roll 0))           [leftHand]
    rightForearm  = Node (Bone 28   (roll 0))           [rightHand]
    leftHand      = Node (Bone 12   (roll 0))           []
    rightHand     = Node (Bone 12   (roll 0))           []
    leftHip       = Node (Bone 19   (roll (pi*9/12)))   [leftThigh]
    rightHip      = Node (Bone 19   (roll (-pi*9/12)))  [rightThigh]
    leftThigh     = Node (Bone 47   (roll (pi*3/12)))   [leftShin]
    rightThigh    = Node (Bone 47   (roll (-pi*3/12)))  [rightShin]
    leftShin      = Node (Bone 38   (roll 0))           [leftFoot]
    rightShin     = Node (Bone 38   (roll 0))           [rightFoot]
    leftFoot      = Node (Bone 14   (pitch (pi*6/12)))  []
    rightFoot     = Node (Bone 14   (pitch (pi*6/12)))  []
