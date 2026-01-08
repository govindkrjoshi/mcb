{-# LANGUAGE NumericUnderscores #-}

-- |
-- Module      : Test.Performance.WithCircuitBreaker
-- Description : Performance tests for withCircuitBreaker
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause
--
-- Performance tests to verify that withCircuitBreaker adds less than 1ms
-- overhead per call under no contention.

module Test.Performance.WithCircuitBreaker (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

import Control.Monad (replicateM_)
import Data.Time.Clock (diffUTCTime, getCurrentTime)

import CircuitBreaker
  ( defaultConfig
  , newCircuitBreaker
  , withCircuitBreaker
  )

tests :: TestTree
tests = testGroup "withCircuitBreaker Performance"
  [ testCase "1000 calls with <1ms overhead per call (total <1s)" $ do
      cb <- newCircuitBreaker defaultConfig
      let numCalls = 1000

      start <- getCurrentTime

      -- Make 1000 calls with trivial action (pure ())
      replicateM_ numCalls $ withCircuitBreaker cb $ pure ()

      end <- getCurrentTime
      let elapsed = diffUTCTime end start
          avgOverhead = elapsed / fromIntegral numCalls

      -- Each call should take less than 1ms on average
      -- Total should be less than 1 second for 1000 calls
      assertBool ("1000 calls took " ++ show elapsed ++ ", avg " ++ show avgOverhead ++ " per call, should be <1ms")
        (avgOverhead < 0.001)  -- 1ms = 0.001 seconds

  , testCase "10000 calls complete in reasonable time (<5s)" $ do
      cb <- newCircuitBreaker defaultConfig
      let numCalls = 10000

      start <- getCurrentTime
      replicateM_ numCalls $ withCircuitBreaker cb $ pure ()
      end <- getCurrentTime

      let elapsed = diffUTCTime end start
          avgOverhead = elapsed / fromIntegral numCalls

      -- 10000 calls should complete in under 5 seconds
      assertBool ("10000 calls took " ++ show elapsed ++ ", should be <5s")
        (elapsed < 5.0)

      -- Average should still be well under 1ms
      assertBool ("Average overhead " ++ show avgOverhead ++ " should be <0.5ms")
        (avgOverhead < 0.0005)  -- 0.5ms

  , testCase "overhead is minimal compared to actual work" $ do
      cb <- newCircuitBreaker defaultConfig
      let numCalls = 100
          work = sum [1..1000 :: Int]  -- Simple computation

      -- Measure time without circuit breaker
      start1 <- getCurrentTime
      replicateM_ numCalls $ do
        let !_ = work
        pure ()
      end1 <- getCurrentTime
      let withoutCB = diffUTCTime end1 start1

      -- Measure time with circuit breaker
      start2 <- getCurrentTime
      replicateM_ numCalls $ withCircuitBreaker cb $ do
        let !_ = work
        pure ()
      end2 <- getCurrentTime
      let withCB = diffUTCTime end2 start2

      let overhead = withCB - withoutCB
          overheadPerCall = overhead / fromIntegral numCalls

      -- Overhead should be less than 1ms per call
      assertBool ("Overhead per call: " ++ show overheadPerCall ++ ", should be <1ms")
        (overheadPerCall < 0.001)

  , testCase "no performance degradation over many calls" $ do
      cb <- newCircuitBreaker defaultConfig

      -- First batch of 1000 calls
      start1 <- getCurrentTime
      replicateM_ 1000 $ withCircuitBreaker cb $ pure ()
      end1 <- getCurrentTime
      let batch1 = diffUTCTime end1 start1

      -- Second batch of 1000 calls
      start2 <- getCurrentTime
      replicateM_ 1000 $ withCircuitBreaker cb $ pure ()
      end2 <- getCurrentTime
      let batch2 = diffUTCTime end2 start2

      -- Third batch of 1000 calls
      start3 <- getCurrentTime
      replicateM_ 1000 $ withCircuitBreaker cb $ pure ()
      end3 <- getCurrentTime
      let batch3 = diffUTCTime end3 start3

      -- Later batches should not be significantly slower
      -- Allow 3x variance for timing noise
      assertBool ("Batch 2 (" ++ show batch2 ++ ") should not be >3x slower than batch 1 (" ++ show batch1 ++ ")")
        (batch2 < batch1 * 3 + 0.01)
      assertBool ("Batch 3 (" ++ show batch3 ++ ") should not be >3x slower than batch 1 (" ++ show batch1 ++ ")")
        (batch3 < batch1 * 3 + 0.01)
  ]
