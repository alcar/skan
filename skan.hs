-- TODO:
-- . INPUT HANDLING
-- .. IMPROVE:
-- ... CHANGE SAVE/EXIT METHOD SO IT DOESN'T NEED UNSAFEPERFORMIO
-- .. IMPLEMENT:
-- ... RENAME ANIMATION (ask name)    KEY = ???
-- ... UNDO       (?)                 KEY = Z
-- ... REDO       (?)                 KEY = X
-- ... HELP MENU  (?)                 KEY = TAB


import Control.Lens
import Data.Foldable (toList)
import Data.Geometry
import Data.List.Util
import Data.Maybe
import Data.Skeleton
import Data.Skeleton.Gloss
import Data.Skeleton.Human
import Data.World
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Linear hiding (trace)
import System.Directory
import System.IO.Unsafe
import qualified Data.Map as Map
import qualified Data.Set as Set

type View = Projection Float


-- INITIALIZATION
-- Initial world state

defaultWorld :: World
defaultWorld
    = World
        0
        Set.empty
        humanSkeletonCursor
        [("UNNAMED", [humanSkeleton])]
        "UNNAMED"
        0
        0


-- VIEWS
-- A view is just a projection from spatial (x,y,z) to screen (x,y,z)
-- On the screen, x goes right, y goes up
-- On the space, x goes right, y goes south, z goes up
-- The char is looking right with left arm pointing north

tibiaView, frontView, topView, sideView :: View
tibiaView   (V3 x y z) = V3 ((x - z*0.707) * 0.8)   ((y + z*0.707) * 0.8)   0
frontView   (V3 x y z) = V3 y                       z                       0
topView     (V3 x y z) = V3 (-y)                    x                       0
sideView    (V3 x y z) = V3 x                       z                       0


-- RENDERING
-- Renders a view

renderView :: World -> View -> Float -> Float -> Picture
renderView world view x y
    = translate x y
        $ renderSkeleton
            view
            (V3 0 0 0)
            (V3 0 0 1)
            (V3 1 0 0)
            (world ^. bonePath)
            (world ^. activeSkeleton)


-- DRAWING
-- Draws the world

draw :: World -> Picture
draw world = pictures [
    renderView world topView    (-100) (-100)   ,
    renderView world frontView  (-100) ( 100)   ,
    renderView world sideView   ( 100) (-100)   ,
    renderView world tibiaView  ( 100) ( 100)   ,
    Translate 80 220 $ Scale 0.1 0.1 $ Text text] where
        text            = animationName ++ " (ANIMATION " ++ animationIndex ++ ") - FRAME " ++ frameIndex
        animationName   = world ^. activeAnimationName
        animationIndex  = show (world ^. activeAnimationIndex)
        frameIndex      = show (world ^. activeFrameIndex)


-- INPUT HANDLING
-- Updates the world after certain inputs

input :: Event -> World -> World
input (EventKey key Up   _ _) = keyDown %~ Set.delete key
input (EventKey key Down _ _)
    = (keyDown %~ Set.insert key)
    . updateActiveAnimationName
    . (Map.findWithDefault id key (Map.fromList [
        (Char 'w'           , moveCursorUp                          ),
        (Char 'a'           , moveCursorLeft                        ),
        (Char 's'           , moveCursorDown                        ),
        (Char 'd'           , moveCursorRight                       ),
        (Char 'r'           , resetActiveBone                       ),
        (Char 'f'           , createNewFrame                        ),
        (Char 'q'           , goToPreviousFrame                     ),
        (Char 'e'           , goToNextFrame                         ),
        (Char 'g'           , deleteActiveFrame                     ),
        (Char 'v'           , createNewAnimation                    ),
        (Char '1'           , goToPreviousAnimation                 ),
        (Char '2'           , goToNextAnimation                     ),
        (Char 'b'           , deleteActiveAnimation                 ),
        (SpecialKey KeyEnter, \ w -> unsafePerformIO (saveAndExit w))]))
input _ = id


-- STATE HANDLING
-- Advances the world to the next state each frame

step :: Float -> World -> World
step dt world = (time +~ dt) . keyboardEffects $ world where
    keyboardEffects = compose $ mapMaybe keyCommand pressedKeys
    keyCommand key  = Map.lookup key commandByKey
    pressedKeys     = Set.toList $ world ^. keyDown
    inc             = 0.02
    commandByKey    = Map.fromList [

        -- Controls the active bone rotation
        (Char 'u', activeBoneRotation *~ roll  (-inc)),
        (Char 'i', activeBoneRotation *~ roll  ( inc)),
        (Char 'j', activeBoneRotation *~ pitch (-inc)),
        (Char 'k', activeBoneRotation *~ pitch ( inc)),
        (Char 'm', activeBoneRotation *~ yaw   (-inc)),
        (Char ',', activeBoneRotation *~ yaw   ( inc))]


-- MAIN MODULE
-- Where the magic happens

main = do
    savedWorldExists <- doesFileExist "savedWorld"
    if savedWorldExists
        then do
            savedWorld <- loadSavedWorld
            play (InWindow "Skeleton_Draw_Test" (512, 512) (0, 0)) white 60 savedWorld draw input step
        else play (InWindow "Skeleton_Draw_Test" (512, 512) (0, 0)) white 60 defaultWorld draw input step
