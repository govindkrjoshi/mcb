{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module      : CircuitBreaker.Bulkhead
-- Description : Bulkhead pattern for limiting concurrent operations
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause
-- Maintainer  : govind@example.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides a bulkhead implementation for limiting the number of
-- concurrent operations. This prevents a single component from consuming all
-- available resources.
--
-- = Bulkhead Pattern
--
-- The bulkhead pattern isolates elements so that if one fails, the others
-- continue to function. Named after ship bulkheads that prevent flooding
-- from spreading across compartments.
--
-- = Usage
--
-- @
-- import CircuitBreaker
--
-- main :: IO ()
-- main = do
--   -- Allow at most 20 concurrent database connections
--   bh <- 'newBulkhead' ('BulkheadConfig' 20)
--   result <- 'withBulkhead' bh $ do
--     queryDatabase
--   print result
-- @
--
-- = Composition with Circuit Breaker
--
-- Bulkheads are typically applied inside the timeout layer:
--
-- @
-- result <- withCircuitBreaker cb $
--             withTimeout (seconds 5) $
--               withBulkhead bh $
--                 queryDatabase
-- @
--
-- @since 0.1.0.0

module CircuitBreaker.Bulkhead
  ( -- * Bulkhead
    Bulkhead
  , BulkheadConfig (..)
  , newBulkhead
  , withBulkhead

    -- * Low-level Operations
  , tryAcquire
  , release
  , getActiveCount
  , getMaxConcurrent
  ) where

import Control.Concurrent.STM
  ( STM
  , TVar
  , atomically
  , newTVarIO
  , readTVar
  , readTVarIO
  , writeTVar
  )
import Control.Monad.Catch (MonadMask, bracket, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)

import CircuitBreaker.Exceptions (BulkheadRejectedException (..))

-- | Configuration for a bulkhead.
--
-- @since 0.1.0.0
data BulkheadConfig = BulkheadConfig
  { bhcMaxConcurrent :: !Int
    -- ^ Maximum number of concurrent operations allowed.
  }
  deriving stock (Eq, Show)

-- | A bulkhead for limiting concurrent operations.
--
-- Create with 'newBulkhead' and use with 'withBulkhead'.
--
-- @since 0.1.0.0
data Bulkhead = Bulkhead
  { bhActiveCount :: !(TVar Int)
    -- ^ Current number of active operations
  , bhConfig :: !BulkheadConfig
    -- ^ Configuration for this bulkhead
  }

-- | Create a new bulkhead with the specified configuration.
--
-- ==== __Examples__
--
-- @
-- -- Allow at most 20 concurrent operations
-- bh <- newBulkhead (BulkheadConfig 20)
--
-- -- Allow at most 5 concurrent operations (more restrictive)
-- bh <- newBulkhead (BulkheadConfig 5)
-- @
--
-- @since 0.1.0.0
newBulkhead :: BulkheadConfig -> IO Bulkhead
newBulkhead config = do
  activeCount <- newTVarIO 0
  pure Bulkhead
    { bhActiveCount = activeCount
    , bhConfig = config
    }

-- | Execute an action with bulkhead protection.
--
-- If a slot is available, the action is executed. If no slots are available,
-- throws 'BulkheadRejectedException'.
--
-- The slot is automatically released when the action completes, whether
-- successfully or with an exception.
--
-- ==== __Examples__
--
-- @
-- bh <- newBulkhead (BulkheadConfig 20)
--
-- -- Execute with concurrency limit
-- result <- withBulkhead bh $ do
--   response <- queryDatabase
--   processResponse response
-- @
--
-- @since 0.1.0.0
withBulkhead
  :: (MonadIO m, MonadMask m)
  => Bulkhead
  -> m a
  -> m a
withBulkhead bh action = bracket acquire releaseSlot (const action)
  where
    acquire = do
      result <- liftIO $ atomically $ tryAcquireSTM bh
      case result of
        Just count -> throwM (BulkheadRejectedException count)
        Nothing -> pure ()

    releaseSlot _ = liftIO $ release bh

-- | Internal: Try to acquire a slot in STM.
-- Returns Nothing on success, Just activeCount on failure.
tryAcquireSTM :: Bulkhead -> STM (Maybe Int)
tryAcquireSTM bh = do
  current <- readTVar (bhActiveCount bh)
  let maxConcurrent = bhcMaxConcurrent (bhConfig bh)
  if current < maxConcurrent
    then do
      writeTVar (bhActiveCount bh) (current + 1)
      pure Nothing
    else pure (Just current)

-- | Try to acquire a slot in the bulkhead.
--
-- Returns 'True' if a slot was successfully acquired,
-- 'False' if no slots are available.
--
-- If this returns 'True', you must call 'release' when done.
-- Prefer 'withBulkhead' which handles this automatically.
--
-- @since 0.1.0.0
tryAcquire :: Bulkhead -> IO Bool
tryAcquire bh = do
  result <- atomically $ tryAcquireSTM bh
  pure (result == Nothing)

-- | Release a slot in the bulkhead.
--
-- This should be called after 'tryAcquire' returns 'True'.
-- Prefer 'withBulkhead' which handles this automatically.
--
-- @since 0.1.0.0
release :: Bulkhead -> IO ()
release bh = atomically $ do
  current <- readTVar (bhActiveCount bh)
  writeTVar (bhActiveCount bh) (max 0 (current - 1))

-- | Get the current number of active operations.
--
-- This is useful for monitoring and debugging.
--
-- @since 0.1.0.0
getActiveCount :: Bulkhead -> IO Int
getActiveCount bh = readTVarIO (bhActiveCount bh)

-- | Get the maximum number of concurrent operations allowed.
--
-- @since 0.1.0.0
getMaxConcurrent :: Bulkhead -> Int
getMaxConcurrent = bhcMaxConcurrent . bhConfig
