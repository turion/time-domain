{- |
This module defines the 'TimeDomain' class.
Its instances model time,
simulated and realtime.
Several instances such as 'UTCTime', 'Double' and 'Integer' are supplied here.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Data.TimeDomain
  ( module Data.TimeDomain
  , UTCTime
  )
  where

-- time
import Data.Time.Clock (UTCTime, diffUTCTime)

{- |
A time domain is an affine space representing a notion of time,
such as real time, simulated time, steps, or a completely different notion.

Expected law:

@(t1 `diffTime` t3) `difference` (t1 `diffTime` t2) = t2 `diffTime` t3@
-}
class TimeDifference (Diff time) => TimeDomain time where
  -- | The type of differences or durations between two timestamps
  type Diff time

  {- | Compute the difference between two timestamps.

  Mnemonic: 'diffTime' behaves like the '(-)' operator:

  @'diffTime' earlier later = later `'diffTime'` earlier@ is the duration it takes from @earlier@ to @later@.
  -}
  diffTime :: time -> time -> Diff time

{- | A type of durations, or differences betweens time stamps.

For the expected law, see 'TimeDomain'.
-}
class TimeDifference d where
  -- | Calculate the difference between two durations,
  --   compatibly with 'diffTime'.
  difference :: d -> d -> d

-- | Differences between 'UTCTime's are measured in seconds.
instance TimeDomain UTCTime where
  type Diff UTCTime = Double
  diffTime t1 t2 = realToFrac $ diffUTCTime t1 t2

instance TimeDifference Double where
  difference = (-)

instance TimeDomain Double where
  type Diff Double = Double
  diffTime = (-)

instance TimeDifference Float where
  difference = (-)

instance TimeDomain Float where
  type Diff Float = Float
  diffTime = (-)

instance TimeDifference Integer where
  difference = (-)

instance TimeDomain Integer where
  type Diff Integer = Integer
  diffTime = (-)

instance TimeDifference () where
  difference _ _ = ()

instance TimeDomain () where
  type Diff () = ()
  diffTime _ _ = ()

-- | Any 'Num' can be wrapped to form a 'TimeDomain'.
newtype NumTimeDomain a = NumTimeDomain { fromNumTimeDomain :: a }
  deriving Num

instance Num a => TimeDifference (NumTimeDomain a) where
  difference = (-)

instance Num a => TimeDomain (NumTimeDomain a) where
  type Diff (NumTimeDomain a) = NumTimeDomain a
  diffTime = (-)
