{-# OPTIONS_GHC -Wno-type-defaults #-}

module TestUtils where

import Task2 (Stream, fromList)
import Data.Foldable (toList)

seconds :: Integer -> Integer
seconds n = n * 10 ^ (6 :: Int)

fibsRef :: [Integer]
fibsRef = 0 : 1 : zipWith (+) fibsRef (tail fibsRef)

fibsRefS :: Stream Integer
fibsRefS = fromList 0 fibsRef

natsRef :: [Integer]
natsRef = [1..]

natsRefS :: Stream Integer
natsRefS = fromList 0 natsRef

isPrime :: Integer -> Bool
isPrime k = (k > 1) && null [ x | x <- [2..floor $ sqrt $ fromIntegral k], k `mod` x == 0]

indices :: Stream Int
indices = fromList 0 [0..]

zipS :: Stream a -> Stream b -> Stream (a, b)
zipS xs ys = fromList undefined $ zip (toList xs) (toList ys)

takeS :: Int -> Stream a -> [a]
takeS n = take n . toList
