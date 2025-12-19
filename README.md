# circuit-breaker

A production-grade circuit breaker library for Haskell.

## Overview

The circuit breaker pattern prevents cascading failures in distributed systems by detecting failures and failing fast when a dependency becomes unavailable.

## Installation

```cabal
build-depends: circuit-breaker
```

## Quick Start

```haskell
import CircuitBreaker

main :: IO ()
main = do
  cb <- newCircuitBreaker defaultConfig
  result <- withCircuitBreaker cb $ do
    -- Your protected operation here
    pure "success"
  print result
```

## Features

- Thread-safe state management using STM
- Configurable failure thresholds and timeouts
- Count-based sliding window for failure rate calculation
- Optional OpenTelemetry integration for observability
- Composable combinators (timeout, fallback)

## Documentation

See [Hackage](https://hackage.haskell.org/package/circuit-breaker) for full API documentation.

## License

BSD-3-Clause - see [LICENSE](LICENSE) for details.
