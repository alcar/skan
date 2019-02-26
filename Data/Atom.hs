module Data.Atom where

import qualified Data.List as L
import Linear

data Atom = Atom {
    atomRad :: !Float,
    atomCha :: !Float,
    atomMas :: !Float,
    atomPos :: !(V3 Float),
    atomVel :: !(V3 Float),
    atomCol :: !(V3 Float)}
    deriving (Show,Eq)

type Interaction = Atom -> Atom -> Atom

data Bind = Bind {
    bindTarget      :: !Atom,
    bindInteraction :: Interaction}

instance Show Bind where
    show (Bind bindTarget _) = show bindTarget

eletricInteraction :: Interaction
eletricInteraction atom other = atom { atomVel = atomVel atom + dir ^* (force / atomMas atom) } where
    force = dist ** 0.5 * atomCha atom * atomCha other
    dir   = normalize (atomPos other - atomPos atom)
    dist  = distance (atomPos atom) (atomPos other)

integrate :: Float -> Atom -> Atom
integrate dt (Atom rad cha mas pos vel col) = Atom rad cha mas (pos + vel ^* dt) vel col

atomStep :: Float -> [Atom] -> Atom -> Atom
atomStep dt atoms atom = integrate dt $ L.foldl' eletricInteraction atom $ filter (/= atom) atoms

renderPixel :: [Atom] -> V3 Float -> V3 Float
renderPixel atoms pos 
    | null touchedAtoms = V3 1 1 1
    | otherwise         = atomCol $ head touchedAtoms
    where 
        touchedAtoms        = filter (isTouching pos) atoms
        isTouching pos atom = distance pos (atomPos atom) < atomRad atom

kineticEnergy :: Atom -> Float
kineticEnergy atom = atomMas atom * (norm (atomVel atom) ** 2) / 2
