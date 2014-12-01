{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, OverloadedLists, LambdaCase #-}

module TallyHo (
  module TallyHo.HyperLogLogC
, module TallyHo.CountMinSketchC
, wordCounter, Result(..), columnarWordCounter
) where

import ClassyPrelude.Conduit

import Data.Char (isSpace)
import Data.Conduit.Combinators (splitOnUnboundedE)
import Data.Vector ((!))

import TallyHo.CountMinSketchC
import TallyHo.HyperLogLogC

data Result = Result { cms :: CountMinSketch Text
                     , card :: Int64 }

-- | Read UTF-8 encoded text from a bytestream source, and count both
-- individual and distinct words.
wordCounter :: (MonadThrow m, MonadBase b m, PrimMonad b) => Sink ByteString m Result
wordCounter = decodeUtf8C $= breakWords $= resultSink

-- | Accept text arrays (of uniform length) and count individual and
-- distinct words per index.  The first array is used to decide how
-- wide the result should be.  If the input array is empty, returns an
-- empty vector.
columnarWordCounter :: (MonadThrow m, MonadBase b m, PrimMonad b) => Sink (Vector Text) m (Vector Result)
columnarWordCounter =
  await >>= \case
    Just as ->
      (yield as >> awaitForever yield) $= fanOutSink (length as) (breakWords $= resultSink)
    Nothing ->
      return mempty

-- | Converts a stream of undifferentiated text-chunks into a stream of words
breakWords :: (Monad m) => Conduit Text m Text
breakWords = splitOnUnboundedE isSpace $= filterC (not . null)

-- | Computes a result from a stream of words.
resultSink :: (MonadBase b m, PrimMonad b) => Sink Text m Result
resultSink = getZipSink $
                    Result <$> (ZipSink $ transPipe liftBase $ countMinSketchC 20 20)
                           <*> (ZipSink $ hyperLogLogC)

-- | Accepts vectors-of-length-n and feeds their elements to n
-- identical sinks, collecting all the results.  Behavior is undefined
-- if the vectors are not all the right length.
fanOutSink :: (Monad m) => Int -> Sink a m b -> Sink (Vector a) m (Vector b)
fanOutSink n s =
  let select_ith i = mapC (! i)
      filters = map select_ith [0..n]
      branches = zipWith ($=) filters (replicate n s)
  in sequenceSinks branches
