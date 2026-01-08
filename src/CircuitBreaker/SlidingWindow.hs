{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module      : CircuitBreaker.SlidingWindow
-- Description : Count-based sliding window for failure rate tracking
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause
-- Maintainer  : govind@example.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module implements a count-based sliding window that tracks the last N
-- operation results (success/failure) for failure rate calculation in the
-- circuit breaker.
--
-- The sliding window uses 'Data.Sequence' for efficient O(1) insertions and
-- removals at both ends, and caches the failure count to provide O(1) failure
-- rate queries.
--
-- = Usage
--
-- @
-- import CircuitBreaker.SlidingWindow
--
-- -- Create a window that tracks the last 10 operations
-- let window = newSlidingWindow 10
--
-- -- Record some results (True = success, False = failure)
-- let window' = insertResult True
--             . insertResult False
--             . insertResult True
--             $ window
--
-- -- Query the failure rate
-- getFailureRate window'  -- Returns 0.333... (1 failure out of 3 calls)
-- @

module CircuitBreaker.SlidingWindow
  ( -- * Data Type
    SlidingWindow

    -- * Construction
  , newSlidingWindow

    -- * Operations
  , insertResult
  , reset

    -- * Queries
  , getFailureRate
  , getTotalCalls
  , isEmpty
  , isFull
  , getWindowSize
  , getFailureCount
  ) where

import Data.Sequence (Seq, ViewL (..), (|>))
import qualified Data.Sequence as Seq

-- | A count-based sliding window for tracking operation results.
--
-- The window maintains a fixed maximum size and uses FIFO ordering,
-- evicting the oldest results when the window is full.
--
-- Implementation details:
--
-- * Uses 'Data.Sequence' for O(1) insertions and removals
-- * Caches failure count to avoid O(n) traversal on queries
-- * Uses strict fields (!) to prevent space leaks
--
-- @since 0.1.0.0
data SlidingWindow = SlidingWindow
  { swResults :: !(Seq Bool)
    -- ^ The sequence of results (True = success, False = failure).
    -- Oldest results are at the front, newest at the back.
  , swWindowSize :: !Int
    -- ^ The maximum number of results to track.
  , swFailureCount :: !Int
    -- ^ Cached count of failures (False values) in the window.
  }
  deriving stock (Eq, Show)

-- | Create a new empty sliding window with the specified maximum size.
--
-- The window size must be positive. If a non-positive size is given,
-- the minimum size of 1 is used.
--
-- ==== __Examples__
--
-- >>> newSlidingWindow 10
-- SlidingWindow {swResults = fromList [], swWindowSize = 10, swFailureCount = 0}
--
-- >>> isEmpty (newSlidingWindow 10)
-- True
--
-- @since 0.1.0.0
newSlidingWindow :: Int -> SlidingWindow
newSlidingWindow size = SlidingWindow
  { swResults = Seq.empty
  , swWindowSize = max 1 size
  , swFailureCount = 0
  }

-- | Insert a result into the sliding window.
--
-- If the window is at capacity, the oldest result is evicted (FIFO).
-- The cached failure count is updated accordingly.
--
-- Results are represented as:
--
-- * 'True' = success
-- * 'False' = failure
--
-- ==== __Examples__
--
-- >>> let w = insertResult False (newSlidingWindow 3)
-- >>> getFailureRate w
-- 1.0
--
-- >>> let w' = insertResult True w
-- >>> getFailureRate w'
-- 0.5
--
-- @since 0.1.0.0
insertResult :: Bool -> SlidingWindow -> SlidingWindow
insertResult result sw
  | isFull sw = evictAndInsert
  | otherwise = justInsert
  where
    -- Add the new result (failure adds 1 to count, success adds 0)
    newFailureIncrement = if result then 0 else 1

    -- Simple insert when not full
    justInsert = sw
      { swResults = swResults sw |> result
      , swFailureCount = swFailureCount sw + newFailureIncrement
      }

    -- Evict oldest and insert new when full
    evictAndInsert = case Seq.viewl (swResults sw) of
      EmptyL -> justInsert  -- Should not happen if isFull is true
      oldest :< rest ->
        let oldestFailureDecrement = if oldest then 0 else 1
        in sw
          { swResults = rest |> result
          , swFailureCount = swFailureCount sw - oldestFailureDecrement + newFailureIncrement
          }

-- | Reset the sliding window, clearing all results.
--
-- The window size is preserved.
--
-- ==== __Examples__
--
-- >>> let w = insertResult False (insertResult True (newSlidingWindow 10))
-- >>> getTotalCalls w
-- 2
-- >>> getTotalCalls (reset w)
-- 0
--
-- @since 0.1.0.0
reset :: SlidingWindow -> SlidingWindow
reset sw = sw
  { swResults = Seq.empty
  , swFailureCount = 0
  }

-- | Get the failure rate as a value between 0.0 and 1.0.
--
-- This is an O(1) operation using the cached failure count.
--
-- Returns 0.0 if the window is empty (no calls to measure).
--
-- ==== __Examples__
--
-- >>> getFailureRate (newSlidingWindow 10)
-- 0.0
--
-- >>> getFailureRate (insertResult False (newSlidingWindow 10))
-- 1.0
--
-- >>> let w = insertResult True (insertResult False (newSlidingWindow 10))
-- >>> getFailureRate w
-- 0.5
--
-- @since 0.1.0.0
getFailureRate :: SlidingWindow -> Double
getFailureRate sw
  | total == 0 = 0.0
  | otherwise = fromIntegral (swFailureCount sw) / fromIntegral total
  where
    total = getTotalCalls sw

-- | Get the total number of calls currently in the window.
--
-- This is always less than or equal to the window size.
--
-- ==== __Examples__
--
-- >>> getTotalCalls (newSlidingWindow 10)
-- 0
--
-- >>> getTotalCalls (insertResult True (newSlidingWindow 10))
-- 1
--
-- @since 0.1.0.0
getTotalCalls :: SlidingWindow -> Int
getTotalCalls = Seq.length . swResults

-- | Check if the window contains no results.
--
-- ==== __Examples__
--
-- >>> isEmpty (newSlidingWindow 10)
-- True
--
-- >>> isEmpty (insertResult True (newSlidingWindow 10))
-- False
--
-- @since 0.1.0.0
isEmpty :: SlidingWindow -> Bool
isEmpty = Seq.null . swResults

-- | Check if the window has reached its maximum capacity.
--
-- ==== __Examples__
--
-- >>> isFull (newSlidingWindow 2)
-- False
--
-- >>> isFull (insertResult True (insertResult True (newSlidingWindow 2)))
-- True
--
-- @since 0.1.0.0
isFull :: SlidingWindow -> Bool
isFull sw = getTotalCalls sw >= swWindowSize sw

-- | Get the maximum window size.
--
-- ==== __Examples__
--
-- >>> getWindowSize (newSlidingWindow 10)
-- 10
--
-- @since 0.1.0.0
getWindowSize :: SlidingWindow -> Int
getWindowSize = swWindowSize

-- | Get the current failure count.
--
-- This is an O(1) operation using the cached count.
--
-- ==== __Examples__
--
-- >>> getFailureCount (newSlidingWindow 10)
-- 0
--
-- >>> getFailureCount (insertResult False (newSlidingWindow 10))
-- 1
--
-- @since 0.1.0.0
getFailureCount :: SlidingWindow -> Int
getFailureCount = swFailureCount
