module Main (main) where

import Criterion.Main
import CircuitBreaker.Timeout (withTimeout, seconds, milliseconds)

main :: IO ()
main = defaultMain
  [ bgroup "withTimeout overhead"
      [ bench "baseline (pure ())" $ nfIO (pure () :: IO ())
      , bench "withTimeout 1s (pure ())" $ nfIO (withTimeout (seconds 1) (pure ()))
      , bench "baseline (pure 42)" $ nfIO (pure (42 :: Int))
      , bench "withTimeout 1s (pure 42)" $ nfIO (withTimeout (seconds 1) (pure 42 :: IO Int))
      , bench "withTimeout 100ms (pure ())" $ nfIO (withTimeout (milliseconds 100) (pure ()))
      ]
  , bgroup "duration helpers"
      [ bench "seconds" $ nf seconds 1.0
      , bench "milliseconds" $ nf milliseconds 100.0
      ]
  ]
