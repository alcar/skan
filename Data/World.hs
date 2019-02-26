{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell #-}

module Data.World where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Lens.Util
import Data.List
import Data.List.Util
import Data.Skeleton
import Data.Skeleton.Gloss
import Data.Skeleton.Human
import Graphics.Gloss.Interface.Pure.Game
import System.Directory
import System.Exit
import qualified Data.Set as Set


-- TYPES AND DATA

-- . Set of keys that are currently pressed
type PressedKeys = Set.Set Key

-- . Animations
type Animation      = [Skeleton Float]
type AnimationList  = [(String, Animation)]

-- . State of the world
data World = World {
    _time                 :: Float            ,
    _keyDown              :: PressedKeys      ,
    _cursor               :: SkeletonCursor   ,
    _animationList        :: AnimationList    ,
    _activeAnimationName  :: String           ,
    _activeAnimationIndex :: Int              ,
    _activeFrameIndex     :: Int              }
makeLenses ''World


-- HELPER LENSES

activeAnimationTuple    = animationList         `thenIx`    activeAnimationIndex
activeAnimation         = activeAnimationTuple  .           _2
activeSkeleton          = activeAnimation       `thenIx`    activeFrameIndex
bonePath                = cursor                .           focus
activeBone              = activeSkeleton        `thenIx`    bonePath
activeBoneRotation      = activeBone            .           boneRot


-- HELPER FUNCTIONS

-- . Loads a previously saved world state
loadSavedWorld :: IO World
loadSavedWorld = do
    savedData <- readFile "savedWorld"
    let dataArray       = lines savedData
        animationList   = read (dataArray !! 0) :: AnimationList
        animationName   =       dataArray !! 1
        animationIndex  = read (dataArray !! 2) :: Int
        frameIndex      = read (dataArray !! 3) :: Int
    return (World 0 Set.empty humanSkeletonCursor animationList animationName animationIndex frameIndex)

-- . Returns the length of the animation list
animationListLength :: World -> Int
animationListLength w = w ^. animationList ^. to length

-- . Returns the length of the active animation
activeAnimationLength :: World -> Int
activeAnimationLength w = w ^. activeAnimation ^. to length

-- . Moves the cursor in a certain direction
moveCursorUp, moveCursorLeft, moveCursorDown, moveCursorRight :: World -> World
moveCursorUp    = cursor %~ view up
moveCursorLeft  = cursor %~ view left
moveCursorDown  = cursor %~ view down
moveCursorRight = cursor %~ view right

-- . Resets the active bone to its default position
resetActiveBone :: World -> World
resetActiveBone w = w & activeBone .~ (humanSkeleton ^?! ix (w ^. bonePath))

-- . Creates a new frame
createNewFrame :: World -> World
createNewFrame w = goToNextFrame $ duplicateActiveFrame w
    where duplicateActiveFrame w = w & activeAnimation %~ duplicateAt (w ^. activeFrameIndex)

-- . Navigates to the previous/next frame
goToPreviousFrame, goToNextFrame :: World -> World
[goToPreviousFrame, goToNextFrame] = map moveFrameIndex [-1, 1]
    where moveFrameIndex = moveBoundedIndex activeFrameIndex activeAnimationLength

-- . Deletes the active frame
deleteActiveFrame :: World -> World
deleteActiveFrame w
  | activeAnimationLength w > 1 = goToPreviousFrame $ w & activeAnimation %~ removeAt (w ^. activeFrameIndex)
  | otherwise                   = w & activeAnimation .~ [humanSkeleton]

-- . Resets the active frame index to 0
resetActiveFrameIndex :: World -> World
resetActiveFrameIndex = activeFrameIndex .~ 0

-- . Creates a new animation
createNewAnimation :: World -> World
createNewAnimation = resetActiveFrameIndex . goToNextAnimation . insertNewAnimation where
    insertNewAnimation w = w & animationList %~ insertAt (w ^. activeAnimationIndex + 1) (newAnimationTuple w)
    newAnimationTuple w = ("UNNAMED", [humanSkeleton])

-- . Navigates to the previous animation
goToPreviousAnimation :: World -> World
goToPreviousAnimation = resetActiveFrameIndex . (moveBoundedIndex activeAnimationIndex animationListLength (-1))

-- . Navigates to the next animation
goToNextAnimation :: World -> World
goToNextAnimation = resetActiveFrameIndex . (moveBoundedIndex activeAnimationIndex animationListLength 1)

-- . Deletes the active animation
deleteActiveAnimation :: World -> World
deleteActiveAnimation = resetActiveFrameIndex . deleteAnimation where
    deleteAnimation w
      | animationListLength w > 1   = w & (animationList %~ removeAt (w ^. activeAnimationIndex)) . changeIndexIfLast
      | otherwise                   = w & activeAnimation .~ [humanSkeleton]
    changeIndexIfLast w
      | w ^. activeAnimationIndex == animationListLength w - 1  = goToPreviousAnimation w
      | otherwise                                               = w

-- . Updates the active animation name to match the active animation
updateActiveAnimationName :: World -> World
updateActiveAnimationName w = w & activeAnimationName .~ w ^. activeAnimationTuple . _1

-- . Saves the state of the world at a given moment in a file named "savedWorld" and terminates the app
saveAndExit :: World -> IO b
saveAndExit w = do
    let worldData = [w ^. animationList ^. to show, w ^. activeAnimationName ^. to show, w ^. activeAnimationIndex ^. to show, w ^. activeFrameIndex ^. to show]
    writeFile "savedWorld" $ concat $ intersperse "\n" worldData
    exitSuccess


-- UNUSED STUFF

{--- . Saves an animation in a .anim file inside the Anims folder-}
{-saveAnimation :: Show a => ([Char], [a]) -> IO () -}
{-saveAnimation animationTuple = writeFile ("Anims/" ++ fst animationTuple ++ ".anim") (saveFrames (snd animationTuple)) where-}
    {-saveFrames []       = ""-}
    {-saveFrames (x:xs)   = show x ++ "\n" ++ saveFrames xs-}

{--- . Saves all the animations open in the current session-}
{-saveAllAnimations :: World -> Int -> IO World-}
{-saveAllAnimations w i = if i < (w ^. animationList ^. to length)-}
    {-then do-}
        {-saveAnimation (w ^. animationList . ix i)-}
        {-saveAllAnimations w (i + 1)-}
    {-else return w-}
