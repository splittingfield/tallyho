{-# LANGUAGE NoImplicitPrelude, RecordWildCards, NamedFieldPuns #-}

module TallyHo.CountMinSketch (
  CountMinSketchM -- Mutable, for initialization
, cmsNew
, cmsAdd
, cmsFreeze
, cmsUnsafeFreeze

, CountMinSketch -- Frozen
, cmsLookup
) where

import ClassyPrelude.Conduit
import Data.Bits ((.&.), shiftL)
import qualified Data.Vector.Primitive.Mutable as PMV
import qualified Data.Vector.Primitive as PM

class CMSLike a where
  wShift :: a -> Int -- shift to multiply by "w"
  wMask :: a -> Int -- mask to do "mod w"
  d :: a -> Int

data CountMinSketchM m a = CountMinSketchM {
  wShiftM :: !Int
, wMaskM :: !Int
, dM :: !Int
, countsM :: !(PMV.MVector (PrimState m) Int)
}

instance CMSLike (CountMinSketchM m a) where
  wShift = wShiftM
  wMask = wMaskM
  d = dM

data CountMinSketch a = CountMinSketch {
  wShiftI :: !Int
, wMaskI :: !Int
, dI :: !Int
, countsI :: !(PM.Vector Int)
}

instance CMSLike (CountMinSketch a) where
  wShift = wShiftI
  wMask = wMaskI
  d = dI

-- | Finds the indexes of the table used by @a@.  This is likely not
-- strictly correct, as 'hashWithSalt' does not guarantee the hash
-- functions indexed by @i@ to be independent, but it's good enough
-- for demo purposes.
cells :: (CMSLike cms, Hashable a) => cms -> a -> [Int]
cells cms a = map hashN [1..d cms]
  where hashN i = (hashWithSalt i a .&. wMask cms) + ((i-1) `shiftL` wShift cms)

cmsAdd :: (PrimMonad m, Hashable a) => CountMinSketchM m a -> a -> m ()
cmsAdd ss@CountMinSketchM{..} a = forM_ (cells ss a) $ \i -> do
  old <- PMV.read countsM i
  PMV.write countsM i (old + 1)

cmsLookup :: (Hashable a) => a -> CountMinSketch a -> Int
cmsLookup a ss@CountMinSketch{..} = foldl' min maxBound $ map (countsI PM.!) $ cells ss a

cmsNew :: (PrimMonad m) => Int -> Int -> m (CountMinSketchM m a)
cmsNew dM wShiftM = do
  let w = 1 `shiftL` wShiftM
      wMaskM = w - 1
  countsM <- PMV.new (dM*w)
  PMV.set countsM 0
  return CountMinSketchM{..}

cmsUnsafeFreeze :: (PrimMonad m) => CountMinSketchM m a -> m (CountMinSketch a)
cmsUnsafeFreeze CountMinSketchM{..} = do
  countsI <- PM.unsafeFreeze countsM
  return $ CountMinSketch { wShiftI = wShiftM
                          , wMaskI = wMaskM
                          , dI = dM
                          , countsI
                          }

cmsFreeze :: (PrimMonad m) => CountMinSketchM m a -> m (CountMinSketch a)
cmsFreeze CountMinSketchM{..} = do
  countsI <- PM.freeze countsM
  return $ CountMinSketch { wShiftI = wShiftM
                          , wMaskI = wMaskM
                          , dI = dM
                          , countsI
                          }
