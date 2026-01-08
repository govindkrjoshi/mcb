{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module      : Main
-- Description : Doctest runner for mcb
-- Copyright   : (c) 2025 Govind
-- License     : BSD-3-Clause
--
-- This module runs doctest on all source files to verify that
-- the examples in Haddock documentation are correct.
--
-- Run with: @cabal test mcb-doctest@

module Main (main) where

import Test.DocTest (mainFromCabal)
import System.Environment (getArgs)

main :: IO ()
main = mainFromCabal "mcb" =<< getArgs
