module Task1Suite where

import TestUtils
import Test.Tasty
import Test.Tasty.HUnit

import Task1 (fibs, nats, primes)

import Control.Monad

task1Tests :: TestTree
task1Tests = testGroup "Task1"
  [ localOption (mkTimeout (seconds 5)) $ testCase "take 1000 nats" $
      forM_ (take 1000 $ zip [0..] (zip nats natsRef)) $ \(idx, (actual, expected)) -> do
        assertEqual ("nats !! " ++ show (idx :: Int)) actual expected

  , localOption (mkTimeout (seconds 5)) $ testCase "take 1000 fibs" $
      forM_ (take 1000 $ zip [0..] (zip fibs fibsRef)) $ \(idx, (actual, expected)) -> do
        assertEqual ("fibs !! " ++ show (idx :: Int)) actual expected

  , localOption (mkTimeout (seconds 5)) $ testCase "take 1000 primes" $
      forM_ (take 1000 $ zip [0..] primes) $ \(idx, actual) -> do
        assertBool ("primes !! " ++ show (idx :: Int) ++ " = " ++ show actual ++ " is not prime") $ True
  ]

