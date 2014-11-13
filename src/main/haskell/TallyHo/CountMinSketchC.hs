{-# LANGUAGE NoImplicitPrelude #-}

module TallyHo.CountMinSketchC (
  module TallyHo.CountMinSketch
, countMinSketchC
) where

import ClassyPrelude.Conduit

import TallyHo.CountMinSketch

countMinSketchC :: (PrimMonad m, Hashable a) => Int -> Int -> Sink a m (CountMinSketch a)
countMinSketchC d wShift = do
  ss <- lift $ cmsNew d wShift
  awaitForever $ lift . cmsAdd ss
  lift $ cmsUnsafeFreeze ss
