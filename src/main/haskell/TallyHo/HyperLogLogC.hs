{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}

module TallyHo.HyperLogLogC (
  hyperLogLogC
) where

import ClassyPrelude.Conduit
import Control.Lens
import Data.Approximate (estimate)
import Data.Digest.Murmur32 (Hashable32, asWord32, hash32)
import Data.HyperLogLog (insert, size, HyperLogLog)

type HLL = HyperLogLog $(12)

-- Hashable32 and preliminary mapC because of https://github.com/ekmett/hyperloglog/issues/8
hyperLogLogC :: (Monad m, Hashable32 a) => Sink a m Int64
hyperLogLogC = mapC (asWord32 . hash32) $= foldlC (flip insert) (mempty :: HLL) <&> size <&> (^.estimate)
