{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where
import Data.Foldable (toList)


-- | Infinite stream of elements
data Stream a = Stream a (Stream a)

instance Foldable Stream where
  foldMap f (Stream x xs) = f x <> foldMap f xs

instance Functor Stream where
    fmap f (Stream x xs) = Stream (f x) (fmap f xs)

instance Show a => Show (Stream a) where
    show s = "First ten elements of stream: " ++ (show . take 10 . toList $ s) ++ "\n"  

-- | Converts given list into stream
--
-- If the list is finite then it is continued
-- with given value repeated infinitely
--
-- Usage example:
--
-- >>> fromList 0 [1,2,3]
-- [1,2,3,0,0,0,0,0,0,0]
-- >>> fromList undefined [1..]
-- [1,2,3,4,5,6,7,8,9,10]
--
fromList :: a -> [a] -> Stream a
fromList z xs = 
    let
        step     = Stream
        d        = Stream z (fromList z [])
    in foldr step d xs

-- | Builds stream from given seed value by applying given step function
--
-- Step function produces a pair of the next element in stream and updated seed value.
--
-- Usage example:
--
-- >>> unfold (\x -> (x, x-1)) 5
-- [5,4,3,2,1,0,-1,-2,-3,-4]
-- >>> unfold (\x -> (abs x, x-1)) 5
-- [5,4,3,2,1,0,1,2,3,4]
--
unfold :: (b -> (a, b)) -> b -> Stream a
unfold f acc =
    let
        next = fst . f $ acc 
        acc' = snd . f $ acc
    in Stream next (unfold f acc')

tail' :: Stream a -> Stream a
tail' (Stream _ xs) = xs

mix :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
mix f (Stream x xs) (Stream y ys) = Stream (f x y) (mix f xs ys)

-- | Returns infinite stream of natural numbers (excluding zero)
--
-- First 10 natural numbers:
--
-- >>> nats
-- [1,2,3,4,5,6,7,8,9,10]
--
nats :: Stream Integer
nats = unfold (\x -> (x, succ x)) 1

-- | Returns infinite stream of fibonacci numbers (starting with zero)
--
-- First 10 fibonacci numbers:
--
-- >>> fibs
-- [0,1,1,2,3,5,8,13,21,34]
--
fibs :: Stream Integer
fibs = unfold (\(u, v) -> (u, (v, u + v))) (0, 1)

-- | Returns infinite stream of prime numbers
--
-- First 10 prime numbers:
--
-- >>> primes
-- [2,3,5,7,11,13,17,19,23,29]
--
primes :: Stream Integer
primes = unfold sieve' (tail' nats)
    where sieve' (Stream p xs) = (p, strikeOut p xs)  

-- | One step of Sieve of Eratosthenes
-- (to be used with 'unfoldr')
--
-- Returns next prime number from given stream
-- and strikes out all multiples of this prime
-- from the rest of the stream
--
-- Usage example:
--
-- >>> sieve $ fromList 0 [2..]
-- (2,[3,5,7,9,11,13,15,17,19,21])
-- >>> sieve $ snd $ sieve $ fromList 0 [2..]
-- (3,[5,7,11,13,17,19,23,25,29,31])
--
sieve :: Stream Integer -> (Integer, Stream Integer)
sieve xs =
    let
        p = findPrime xs
    in (p, strikeOut p xs)


strikeOut :: Integer -> Stream Integer -> Stream Integer
strikeOut p = foldr step undefined 
    where step x acc = if x `mod` p == 0 then acc else Stream x acc 

findPrime :: Stream Integer -> Integer 
findPrime (Stream x xs) = if isPrime x then x else findPrime xs 


isPrime :: Integer -> Bool
isPrime n
  | n < 2     = False
  | otherwise = null [ x | x <- [2 .. (floor :: Double -> Integer). sqrt . fromIntegral $ n], n `mod` x == 0 ]
