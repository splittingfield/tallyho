{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module TallyHo.All (
  module TallyHo.HyperLogLogC
, module TallyHo.CountMinSketchC
, x, Result(..)
) where

import ClassyPrelude.Conduit
import Data.Text (splitOn)

import TallyHo.HyperLogLogC
import TallyHo.CountMinSketchC

data Result = Result { cms :: CountMinSketch Text
                     , card :: Int64 }

-- | Read UTF-8 encoded text from a bytestream source, and count both
-- individual and distinct words.
x :: (MonadThrow (t m), MonadTrans t, PrimMonad m) => Sink ByteString (t m) Result
x = decodeUtf8C $= linesUnboundedC
                $= awaitForever (mapM_ yield . splitOn " ")
                $= filterC (/= "")
                $= (getZipSink $
                      Result <$> (ZipSink $ transPipe lift $ countMinSketchC 20 20)
                             <*> (ZipSink $ mapC unpack $= hyperLogLogC))
