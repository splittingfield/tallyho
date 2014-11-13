{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module TallyHo (
  module TallyHo.HyperLogLogC
, module TallyHo.CountMinSketchC
, wordCounter, Result(..)
) where

import ClassyPrelude.Conduit
import Data.Text (splitOn)

import TallyHo.HyperLogLogC
import TallyHo.CountMinSketchC

data Result = Result { cms :: CountMinSketch Text
                     , card :: Int64 }

-- | Read UTF-8 encoded text from a bytestream source, and count both
-- individual and distinct words.
wordCounter :: (MonadThrow m, MonadBase b m, PrimMonad b) => Sink ByteString m Result
wordCounter =
  decodeUtf8C $= linesUnboundedC
              $= awaitForever (mapM_ yield . splitOn " ")
              $= filterC (/= "")
              $= (getZipSink $
                    Result <$> (ZipSink $ transPipe liftBase $ countMinSketchC 20 20)
                           <*> (ZipSink $ mapC unpack $= hyperLogLogC))
