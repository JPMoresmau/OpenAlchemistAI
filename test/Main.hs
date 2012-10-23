-- | test entry point
module Main where

import Games.OpenAlchemist.AI.GameTests

import Test.Framework (defaultMain, testGroup,Test)
import Test.Framework.Providers.HUnit

main :: IO()
main = defaultMain tests

tests :: [Test]
tests = [
        testGroup "Game Tests" (concatMap hUnitTestToTests gameTests)
        ]