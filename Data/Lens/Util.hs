{-# LANGUAGE RankNTypes #-}

module Data.Lens.Util where

import Control.Lens


-- HELPER FUNCTIONS

-- . Goes to `baseLens`, and then goes to an index that is addressed by `indexLens`
thenIx :: (Ixed b) =>  Lens' a b -> Lens' a (Index b) -> Lens' a (IxValue b)
thenIx baseLens indexLens = lens getter setter where
    getter object           = object ^?! baseLens . ix (object ^. indexLens)
    setter object newValue  = object & baseLens . ix (object ^. indexLens) .~ newValue

-- . Adds delta to an object focused by an index lens, within the specified boundaries
moveBoundedIndex :: Lens' a Int -> (a -> Int) -> Int -> a -> a
moveBoundedIndex indexLens bound delta obj = obj & indexLens %~ (max 0 . min (bound obj - 1) . (+ delta))
