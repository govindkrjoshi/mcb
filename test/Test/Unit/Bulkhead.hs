-- |
-- Module      : Test.Unit.Bulkhead
-- Description : Unit tests for Bulkhead
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause

module Test.Unit.Bulkhead
  ( tests
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (ErrorCall, SomeException, try)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

import CircuitBreaker
  ( Bulkhead
  , BulkheadConfig (..)
  , BulkheadRejectedException (..)
  , getActiveCount
  , getMaxConcurrent
  , newBulkhead
  , release
  , tryAcquire
  , withBulkhead
  )

tests :: TestTree
tests = testGroup "CircuitBreaker.Bulkhead"
  [ configTests
  , newBulkheadTests
  , tryAcquireTests
  , withBulkheadTests
  , concurrencyTests
  ]

configTests :: TestTree
configTests = testGroup "BulkheadConfig"
  [ testCase "can create config with max concurrent" $ do
      let config = BulkheadConfig 20
      bhcMaxConcurrent config @?= 20

  , testCase "config has Show instance" $ do
      let config = BulkheadConfig 20
      length (show config) > 0 @?= True

  , testCase "config has Eq instance" $ do
      let config1 = BulkheadConfig 20
          config2 = BulkheadConfig 20
          config3 = BulkheadConfig 30
      assertEqual "same configs equal" config1 config2
      assertBool "different configs not equal" (config1 /= config3)
  ]

newBulkheadTests :: TestTree
newBulkheadTests = testGroup "newBulkhead"
  [ testCase "creates bulkhead with zero active count" $ do
      bh <- newBulkhead (BulkheadConfig 20)
      count <- getActiveCount bh
      count @?= 0

  , testCase "getMaxConcurrent returns configured value" $ do
      bh <- newBulkhead (BulkheadConfig 50)
      getMaxConcurrent bh @?= 50
  ]

tryAcquireTests :: TestTree
tryAcquireTests = testGroup "tryAcquire/release"
  [ testCase "acquire succeeds when slots available" $ do
      bh <- newBulkhead (BulkheadConfig 5)
      success <- tryAcquire bh
      assertBool "should acquire successfully" success
      release bh  -- Clean up

  , testCase "acquire fails when no slots available" $ do
      bh <- newBulkhead (BulkheadConfig 1)
      _ <- tryAcquire bh  -- Take the only slot
      success <- tryAcquire bh  -- Try to take another
      assertBool "should fail to acquire" (not success)
      release bh  -- Clean up

  , testCase "release frees up a slot" $ do
      bh <- newBulkhead (BulkheadConfig 1)
      _ <- tryAcquire bh
      release bh
      success <- tryAcquire bh
      assertBool "should acquire after release" success
      release bh

  , testCase "active count tracks acquisitions" $ do
      bh <- newBulkhead (BulkheadConfig 10)
      _ <- tryAcquire bh
      _ <- tryAcquire bh
      _ <- tryAcquire bh
      count <- getActiveCount bh
      count @?= 3
      release bh
      count' <- getActiveCount bh
      count' @?= 2
      release bh
      release bh
  ]

withBulkheadTests :: TestTree
withBulkheadTests = testGroup "withBulkhead"
  [ testCase "action executes when slots available" $ do
      bh <- newBulkhead (BulkheadConfig 5)
      result <- withBulkhead bh (pure "success" :: IO String)
      result @?= "success"

  , testCase "throws BulkheadRejectedException when full" $ do
      bh <- newBulkhead (BulkheadConfig 1)
      _ <- tryAcquire bh  -- Fill the bulkhead
      result <- try $ withBulkhead bh (pure "success" :: IO String)
      case result of
        Left (BulkheadRejectedException count) -> count @?= 1
        Right _ -> error "Expected BulkheadRejectedException"
      release bh

  , testCase "slot released after action completes" $ do
      bh <- newBulkhead (BulkheadConfig 1)
      _ <- withBulkhead bh (pure () :: IO ())
      count <- getActiveCount bh
      count @?= 0

  , testCase "slot released after action throws" $ do
      bh <- newBulkhead (BulkheadConfig 1)
      _ <- try $ withBulkhead bh (error "test" :: IO ()) :: IO (Either SomeException ())
      count <- getActiveCount bh
      count @?= 0

  , testCase "action exception propagates through" $ do
      bh <- newBulkhead (BulkheadConfig 5)
      result <- try $ withBulkhead bh (error "test error" :: IO String)
      case result of
        Left e -> assertBool "should propagate error" ("test" `elem` words (show (e :: SomeException)))
        Right _ -> error "Expected exception"
  ]

concurrencyTests :: TestTree
concurrencyTests = testGroup "Concurrency"
  [ testCase "concurrent requests up to limit succeed" $ do
      bh <- newBulkhead (BulkheadConfig 3)
      startSignal <- newEmptyMVar
      endSignals <- sequence $ replicate 3 newEmptyMVar

      -- Start 3 concurrent operations
      sequence_
        [ forkIO $ do
            _ <- withBulkhead bh $ do
              putMVar (endSignals !! i) ()  -- Signal started
              takeMVar startSignal  -- Wait for release signal
              pure ()
            pure ()
        | i <- [0..2]
        ]

      -- Wait for all to start
      sequence_ $ map takeMVar endSignals

      -- All 3 should be running
      count <- getActiveCount bh
      count @?= 3

      -- Let them all complete
      putMVar startSignal ()
      putMVar startSignal ()
      putMVar startSignal ()

      threadDelay 10000  -- Let them finish
      finalCount <- getActiveCount bh
      finalCount @?= 0
  ]
