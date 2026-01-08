{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module      : Test.Unit.Predicate
-- Description : Unit tests for exception predicate helpers
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause
--
-- Unit tests for the CircuitBreaker.Predicate module, testing
-- default predicates, type-specific predicates, and combinators.

module Test.Unit.Predicate (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Control.Exception (Exception, SomeException, toException)
import Control.Monad (replicateM_)
import Control.Monad.Catch (catch, throwM)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)

import CircuitBreaker
  ( State (..)
  , defaultConfig
  , defaultExceptionPredicate
  , getCurrentState
  , ignoreException
  , onlyException
  , matchException
  , combinePredicates
  , anyPredicate
  , allPredicate
  , ignoreExceptions
  , onlyExceptions
  , negatePredicate
  , whenMatches
  , newCircuitBreaker
  , setExceptionPredicate
  , setFailureThreshold
  , setSlidingWindowSize
  , withCircuitBreaker
  , (&)
  )

-- ---------------------------------------------------------------------------
-- Test Exception Types
-- ---------------------------------------------------------------------------

-- | A "client error" exception that should typically be ignored
data ClientException = ClientException String
  deriving stock (Eq, Show, Typeable)

instance Exception ClientException

-- | A "server error" exception that should count as a failure
data ServerException = ServerException String
  deriving stock (Eq, Show, Typeable)

instance Exception ServerException

-- | A timeout exception
data TimeoutException = TimeoutException
  deriving stock (Eq, Show, Typeable)

instance Exception TimeoutException

-- | A validation exception (business logic error)
data ValidationException = ValidationException String
  deriving stock (Eq, Show, Typeable)

instance Exception ValidationException

-- | An HTTP-like exception with status code
data HttpException = HttpException Int String
  deriving stock (Eq, Show, Typeable)

instance Exception HttpException

-- ---------------------------------------------------------------------------
-- Test Suite
-- ---------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "CircuitBreaker.Predicate"
  [ defaultPredicateTests
  , ignoreExceptionTests
  , onlyExceptionTests
  , matchExceptionTests
  , combinePredicatesTests
  , anyPredicateTests
  , allPredicateTests
  , ignoreExceptionsTests
  , onlyExceptionsTests
  , negatePredicateTests
  , whenMatchesTests
  , integrationTests
  ]

-- ---------------------------------------------------------------------------
-- Default Predicate Tests
-- ---------------------------------------------------------------------------

defaultPredicateTests :: TestTree
defaultPredicateTests = testGroup "defaultExceptionPredicate"
  [ testCase "returns True for any exception" $ do
      let clientEx = toException (ClientException "client error")
          serverEx = toException (ServerException "server error")
          timeoutEx = toException TimeoutException
      defaultExceptionPredicate clientEx @?= True
      defaultExceptionPredicate serverEx @?= True
      defaultExceptionPredicate timeoutEx @?= True

  , testCase "is equivalent to const True" $ do
      let ex = toException (ClientException "test")
      defaultExceptionPredicate ex @?= const True ex
  ]

-- ---------------------------------------------------------------------------
-- ignoreException Tests
-- ---------------------------------------------------------------------------

ignoreExceptionTests :: TestTree
ignoreExceptionTests = testGroup "ignoreException"
  [ testCase "returns False for matching exception type" $ do
      let predicate = ignoreException (Proxy :: Proxy ClientException)
          ex = toException (ClientException "ignored")
      predicate ex @?= False

  , testCase "returns True for non-matching exception type" $ do
      let predicate = ignoreException (Proxy :: Proxy ClientException)
          ex = toException (ServerException "not ignored")
      predicate ex @?= True

  , testCase "works with different exception types" $ do
      let predicateClient = ignoreException (Proxy :: Proxy ClientException)
          predicateServer = ignoreException (Proxy :: Proxy ServerException)
          clientEx = toException (ClientException "client")
          serverEx = toException (ServerException "server")

      predicateClient clientEx @?= False  -- Client ignored by client predicate
      predicateClient serverEx @?= True   -- Server not ignored by client predicate
      predicateServer serverEx @?= False  -- Server ignored by server predicate
      predicateServer clientEx @?= True   -- Client not ignored by server predicate
  ]

-- ---------------------------------------------------------------------------
-- onlyException Tests
-- ---------------------------------------------------------------------------

onlyExceptionTests :: TestTree
onlyExceptionTests = testGroup "onlyException"
  [ testCase "returns True for matching exception type" $ do
      let predicate = onlyException (Proxy :: Proxy ServerException)
          ex = toException (ServerException "failure")
      predicate ex @?= True

  , testCase "returns False for non-matching exception type" $ do
      let predicate = onlyException (Proxy :: Proxy ServerException)
          ex = toException (ClientException "not a failure")
      predicate ex @?= False

  , testCase "is opposite of ignoreException for same type" $ do
      let ignorePred = ignoreException (Proxy :: Proxy ServerException)
          onlyPred = onlyException (Proxy :: Proxy ServerException)
          serverEx = toException (ServerException "test")
          clientEx = toException (ClientException "test")

      -- For matching type: ignore returns False, only returns True
      ignorePred serverEx @?= not (onlyPred serverEx)
      -- For non-matching type: ignore returns True, only returns False
      ignorePred clientEx @?= not (onlyPred clientEx)
  ]

-- ---------------------------------------------------------------------------
-- matchException Tests
-- ---------------------------------------------------------------------------

matchExceptionTests :: TestTree
matchExceptionTests = testGroup "matchException"
  [ testCase "returns True when exception matches type" $ do
      let ex = toException (ServerException "test")
      matchException (Proxy :: Proxy ServerException) ex @?= True

  , testCase "returns False when exception does not match type" $ do
      let ex = toException (ClientException "test")
      matchException (Proxy :: Proxy ServerException) ex @?= False

  , testCase "can match multiple types independently" $ do
      let ex = toException TimeoutException
      matchException (Proxy :: Proxy TimeoutException) ex @?= True
      matchException (Proxy :: Proxy ServerException) ex @?= False
      matchException (Proxy :: Proxy ClientException) ex @?= False
  ]

-- ---------------------------------------------------------------------------
-- combinePredicates Tests
-- ---------------------------------------------------------------------------

combinePredicatesTests :: TestTree
combinePredicatesTests = testGroup "combinePredicates"
  [ testCase "returns True when all predicates return True" $ do
      let predicates = [const True, const True, const True]
          ex = toException (ServerException "test")
      combinePredicates predicates ex @?= True

  , testCase "returns False when any predicate returns False" $ do
      let predicates = [const True, const False, const True]
          ex = toException (ServerException "test")
      combinePredicates predicates ex @?= False

  , testCase "returns True for empty predicate list" $ do
      let ex = toException (ServerException "test")
      combinePredicates [] ex @?= True

  , testCase "combines real predicates correctly" $ do
      -- Only count ServerException that is also NOT a ClientException
      -- (This is a contrived example since an exception can't be both)
      let predicates =
            [ onlyException (Proxy :: Proxy ServerException)
            , ignoreException (Proxy :: Proxy ClientException)
            ]
          serverEx = toException (ServerException "test")
          clientEx = toException (ClientException "test")

      -- ServerException: only returns True, ignore returns True => True
      combinePredicates predicates serverEx @?= True
      -- ClientException: only returns False, ignore returns False => False
      combinePredicates predicates clientEx @?= False
  ]

-- ---------------------------------------------------------------------------
-- anyPredicate Tests
-- ---------------------------------------------------------------------------

anyPredicateTests :: TestTree
anyPredicateTests = testGroup "anyPredicate"
  [ testCase "returns True when any predicate returns True" $ do
      let predicates = [const False, const True, const False]
          ex = toException (ServerException "test")
      anyPredicate predicates ex @?= True

  , testCase "returns False when all predicates return False" $ do
      let predicates = [const False, const False, const False]
          ex = toException (ServerException "test")
      anyPredicate predicates ex @?= False

  , testCase "returns False for empty predicate list" $ do
      let ex = toException (ServerException "test")
      anyPredicate [] ex @?= False

  , testCase "works with real exception type predicates" $ do
      let predicates =
            [ onlyException (Proxy :: Proxy ServerException)
            , onlyException (Proxy :: Proxy TimeoutException)
            ]
          serverEx = toException (ServerException "test")
          timeoutEx = toException TimeoutException
          clientEx = toException (ClientException "test")

      anyPredicate predicates serverEx @?= True
      anyPredicate predicates timeoutEx @?= True
      anyPredicate predicates clientEx @?= False
  ]

-- ---------------------------------------------------------------------------
-- allPredicate Tests
-- ---------------------------------------------------------------------------

allPredicateTests :: TestTree
allPredicateTests = testGroup "allPredicate"
  [ testCase "is equivalent to combinePredicates" $ do
      let predicates = [const True, const False, const True]
          ex = toException (ServerException "test")
      allPredicate predicates ex @?= combinePredicates predicates ex

  , testCase "returns True for empty list" $ do
      let ex = toException (ServerException "test")
      allPredicate [] ex @?= True
  ]

-- ---------------------------------------------------------------------------
-- ignoreExceptions Tests
-- ---------------------------------------------------------------------------

ignoreExceptionsTests :: TestTree
ignoreExceptionsTests = testGroup "ignoreExceptions"
  [ testCase "ignores multiple exception types" $ do
      let predicate = ignoreExceptions
            [ ignoreException (Proxy :: Proxy ClientException)
            , ignoreException (Proxy :: Proxy ValidationException)
            ]
          clientEx = toException (ClientException "client")
          validationEx = toException (ValidationException "validation")
          serverEx = toException (ServerException "server")

      -- Both ClientException and ValidationException should be ignored
      predicate clientEx @?= False
      predicate validationEx @?= False
      -- ServerException should count as failure
      predicate serverEx @?= True

  , testCase "with empty list counts everything as failure" $ do
      let predicate = ignoreExceptions []
          ex = toException (ClientException "test")
      predicate ex @?= True
  ]

-- ---------------------------------------------------------------------------
-- onlyExceptions Tests
-- ---------------------------------------------------------------------------

onlyExceptionsTests :: TestTree
onlyExceptionsTests = testGroup "onlyExceptions"
  [ testCase "counts multiple exception types as failures" $ do
      let predicate = onlyExceptions
            [ onlyException (Proxy :: Proxy ServerException)
            , onlyException (Proxy :: Proxy TimeoutException)
            ]
          serverEx = toException (ServerException "server")
          timeoutEx = toException TimeoutException
          clientEx = toException (ClientException "client")

      -- Both ServerException and TimeoutException should count
      predicate serverEx @?= True
      predicate timeoutEx @?= True
      -- ClientException should be ignored
      predicate clientEx @?= False

  , testCase "with empty list ignores everything" $ do
      let predicate = onlyExceptions []
          ex = toException (ServerException "test")
      predicate ex @?= False
  ]

-- ---------------------------------------------------------------------------
-- negatePredicate Tests
-- ---------------------------------------------------------------------------

negatePredicateTests :: TestTree
negatePredicateTests = testGroup "negatePredicate"
  [ testCase "flips True to False" $ do
      let original = const True
          negated = negatePredicate original
          ex = toException (ServerException "test")
      negated ex @?= False

  , testCase "flips False to True" $ do
      let original = const False
          negated = negatePredicate original
          ex = toException (ServerException "test")
      negated ex @?= True

  , testCase "negating ignoreException is like onlyException" $ do
      let ignorePred = ignoreException (Proxy :: Proxy ServerException)
          negatedIgnore = negatePredicate ignorePred
          onlyPred = onlyException (Proxy :: Proxy ServerException)
          serverEx = toException (ServerException "test")
          clientEx = toException (ClientException "test")

      negatedIgnore serverEx @?= onlyPred serverEx
      negatedIgnore clientEx @?= onlyPred clientEx

  , testCase "double negation returns original result" $ do
      let original = onlyException (Proxy :: Proxy ServerException)
          doubleNegated = negatePredicate (negatePredicate original)
          serverEx = toException (ServerException "test")
          clientEx = toException (ClientException "test")

      doubleNegated serverEx @?= original serverEx
      doubleNegated clientEx @?= original clientEx
  ]

-- ---------------------------------------------------------------------------
-- whenMatches Tests
-- ---------------------------------------------------------------------------

whenMatchesTests :: TestTree
whenMatchesTests = testGroup "whenMatches"
  [ testCase "applies check function when exception matches" $ do
      let predicate = whenMatches (Proxy :: Proxy HttpException)
            (\(HttpException status _) -> status >= 500)
            True
          http500 = toException (HttpException 500 "Internal Server Error")
          http404 = toException (HttpException 404 "Not Found")

      predicate http500 @?= True   -- 500 >= 500
      predicate http404 @?= False  -- 404 < 500

  , testCase "returns default for non-matching exceptions" $ do
      let predicateDefaultTrue = whenMatches (Proxy :: Proxy HttpException)
            (\(HttpException status _) -> status >= 500)
            True
          predicateDefaultFalse = whenMatches (Proxy :: Proxy HttpException)
            (\(HttpException status _) -> status >= 500)
            False
          serverEx = toException (ServerException "test")

      predicateDefaultTrue serverEx @?= True
      predicateDefaultFalse serverEx @?= False

  , testCase "can inspect exception message" $ do
      let predicate = whenMatches (Proxy :: Proxy ServerException)
            (\(ServerException msg) -> msg == "critical")
            True
          critical = toException (ServerException "critical")
          minor = toException (ServerException "minor")

      predicate critical @?= True
      predicate minor @?= False

  , testCase "example: only count 5xx HTTP errors" $ do
      let isServerError = whenMatches (Proxy :: Proxy HttpException)
            (\(HttpException status _) -> status >= 500)
            True  -- Unknown exceptions count as failures

          http200 = toException (HttpException 200 "OK")
          http400 = toException (HttpException 400 "Bad Request")
          http404 = toException (HttpException 404 "Not Found")
          http500 = toException (HttpException 500 "Internal Server Error")
          http502 = toException (HttpException 502 "Bad Gateway")
          otherEx = toException TimeoutException

      isServerError http200 @?= False
      isServerError http400 @?= False
      isServerError http404 @?= False
      isServerError http500 @?= True
      isServerError http502 @?= True
      isServerError otherEx @?= True  -- Default behavior for unknown exceptions
  ]

-- ---------------------------------------------------------------------------
-- Integration Tests with Circuit Breaker
-- ---------------------------------------------------------------------------

integrationTests :: TestTree
integrationTests = testGroup "Integration with Circuit Breaker"
  [ testCase "business exceptions don't open circuit when ignored" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setExceptionPredicate (ignoreException (Proxy :: Proxy ClientException))
      cb <- newCircuitBreaker config

      -- 10 client exceptions should not open circuit
      replicateM_ 10 $ do
        (withCircuitBreaker cb $ throwM (ClientException "ignored") :: IO ())
          `catch` \(_ :: ClientException) -> pure ()

      state <- getCurrentState cb
      state @?= Closed

  , testCase "server exceptions open circuit when not ignored" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setExceptionPredicate (ignoreException (Proxy :: Proxy ClientException))
      cb <- newCircuitBreaker config

      -- 5 server exceptions should open circuit (50% threshold)
      replicateM_ 5 $ do
        (withCircuitBreaker cb $ throwM (ServerException "failure") :: IO ())
          `catch` \(_ :: ServerException) -> pure ()

      state <- getCurrentState cb
      state @?= Open

  , testCase "mixed exceptions with selective counting" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setExceptionPredicate (onlyExceptions
                [ onlyException (Proxy :: Proxy ServerException)
                , onlyException (Proxy :: Proxy TimeoutException)
                ])
      cb <- newCircuitBreaker config

      -- 5 client exceptions - should not count
      replicateM_ 5 $ do
        (withCircuitBreaker cb $ throwM (ClientException "client") :: IO ())
          `catch` \(_ :: ClientException) -> pure ()

      -- 3 server exceptions - should count (only 3 of 8 = 37.5% < 50%)
      replicateM_ 3 $ do
        (withCircuitBreaker cb $ throwM (ServerException "server") :: IO ())
          `catch` \(_ :: ServerException) -> pure ()

      state1 <- getCurrentState cb
      state1 @?= Closed

      -- 2 more server exceptions - should count (5 of 10 = 50% >= 50%)
      replicateM_ 2 $ do
        (withCircuitBreaker cb $ throwM (ServerException "server") :: IO ())
          `catch` \(_ :: ServerException) -> pure ()

      state2 <- getCurrentState cb
      state2 @?= Open

  , testCase "whenMatches predicate filters HTTP errors by status" $ do
      let isServerError = whenMatches (Proxy :: Proxy HttpException)
            (\(HttpException status _) -> status >= 500)
            True

          config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setExceptionPredicate isServerError
      cb <- newCircuitBreaker config

      -- 10 HTTP 404 errors should not open circuit
      replicateM_ 10 $ do
        (withCircuitBreaker cb $ throwM (HttpException 404 "Not Found") :: IO ())
          `catch` \(_ :: HttpException) -> pure ()

      state1 <- getCurrentState cb
      state1 @?= Closed

      -- 5 HTTP 500 errors should open circuit
      replicateM_ 5 $ do
        (withCircuitBreaker cb $ throwM (HttpException 500 "Server Error") :: IO ())
          `catch` \(_ :: HttpException) -> pure ()

      state2 <- getCurrentState cb
      state2 @?= Open

  , testCase "ignored exceptions are still propagated to caller" $ do
      let config = defaultConfig
            & setExceptionPredicate (ignoreException (Proxy :: Proxy ClientException))
      cb <- newCircuitBreaker config

      caught <- (withCircuitBreaker cb $ throwM (ClientException "test") >> pure False)
        `catch` \(ClientException msg) -> pure (msg == "test")

      assertBool "Exception should be propagated with original message" caught

  , testCase "defaultExceptionPredicate counts all exceptions" $ do
      let config = defaultConfig
            & setFailureThreshold 0.5
            & setSlidingWindowSize 10
            & setExceptionPredicate defaultExceptionPredicate
      cb <- newCircuitBreaker config

      -- Mix of different exceptions
      replicateM_ 2 $ do
        (withCircuitBreaker cb $ throwM (ClientException "client") :: IO ())
          `catch` \(_ :: ClientException) -> pure ()
      replicateM_ 2 $ do
        (withCircuitBreaker cb $ throwM (ServerException "server") :: IO ())
          `catch` \(_ :: ServerException) -> pure ()
      replicateM_ 1 $ do
        (withCircuitBreaker cb $ throwM TimeoutException :: IO ())
          `catch` \(_ :: TimeoutException) -> pure ()

      -- 5 failures of 5 calls = 100% > 50%
      state <- getCurrentState cb
      state @?= Open
  ]
