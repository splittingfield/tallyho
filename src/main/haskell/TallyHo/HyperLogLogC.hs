{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}

module TallyHo.HyperLogLogC (
  hyperLogLogC
) where

import ClassyPrelude.Conduit
import Control.Lens
import Data.Approximate (estimate)
import Data.HyperLogLog (insert, size, HyperLogLog)
import Data.Bytes.Serial (Serial)

type HLL = HyperLogLog $(12)

hyperLogLogC :: (Monad m, Serial a) => Sink a m Int64
hyperLogLogC = foldlC (flip insert) (mempty :: HLL) <&> size <&> (^.estimate)
