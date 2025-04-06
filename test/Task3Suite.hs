module Task3Suite where

import TestUtils
import Test.Tasty
import Test.Tasty.HUnit

import Task3 (fibs, nats, ones)

import Control.Monad

task3Tests :: TestTree
task3Tests = testGroup "Task3"
  [ localOption (mkTimeout (seconds 5)) $ testCase "take 1000 nats" $
      forM_ (takeS 1000 $ zipS indices (zipS nats natsRefS)) $ \(idx, (actual, expected)) -> do
        assertEqual ("nats #" ++ show idx) actual expected

  , localOption (mkTimeout (seconds 5)) $ testCase "take 1000 fibs" $
      forM_ (takeS 1000 $ zipS indices (zipS fibs fibsRefS)) $ \(idx, (actual, expected)) -> do
        assertEqual ("fibs #" ++ show idx) actual expected

  , localOption (mkTimeout (seconds 5)) $ testCase "take 1000 ones" $
      forM_ (takeS 1000 $ zipS indices ones) $ \(idx, actual) -> do
        assertEqual ("ones #" ++ show idx) actual 1
  ]
