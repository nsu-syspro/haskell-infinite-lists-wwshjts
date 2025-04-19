{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3 where

import Task2 hiding (nats, primes, fibs)
import Data.Ratio (Ratio)
import GHC.Real (numerator)

-- | Power series represented as infinite stream of coefficients
-- 
-- For following series
--   @a0 + a1 * x + a2 * x^2 + ...@
-- coefficients would be
--   @a0, a1, a2, ...@
--
-- Usage examples:
--
-- >>> coefficients (x + x ^ 2 + x ^ 4)
-- [0,1,1,0,1,0,0,0,0,0]
-- >>> coefficients ((1 + x)^5)
-- [1,5,10,10,5,1,0,0,0,0]
-- >>> coefficients (42 :: Series Integer)
-- [42,0,0,0,0,0,0,0,0,0]
--
newtype Series a = Series
  { coefficients :: Stream a
  -- ^ Returns coefficients of given power series
  --
  -- For following series
  --   @a0 + a1 * x + a2 * x^2 + ...@
  -- coefficients would be
  --   @a0, a1, a2, ...@
  }

-- | Power series corresponding to single @x@
--
-- First 10 coefficients:
--
-- >>> coefficients x
-- [0,1,0,0,0,0,0,0,0,0]
--

instance Num a => Num (Series a) where
    (Series xs) + (Series ys) = Series (mix (+) xs ys)

    (Series (Stream a0 as)) * l@(Series (Stream b0 bs)) =
        fromNumber (a0 * b0) + shiftRight (mul + Series as * l)
        where
            mul = a0 *: Series bs 

    abs    (Series xs) = Series (abs    <$> xs)
    signum (Series xs) = Series (signum <$> xs)
    negate (Series xs) = Series (negate <$> xs)
    fromInteger n = Series (fromList 0 [fromInteger n])

instance Fractional a => Fractional (Series a) where
    fromRational q = Series (fromList 0 [fromRational q]) 
    (Series (Stream a as)) / l@(Series (Stream b bs)) = 
        fromFractional q + shiftRight ((Series as - Series mul) / l)
        where q   = a / b
              mul = (*) q <$> bs 

x :: Num a => Series a
x =  Series (fromList 0 [0, 1])

fromFractional :: Fractional a => a -> Series a
fromFractional q = Series (fromList 0 [q])

fromNumber :: Num a => a -> Series a
fromNumber n = Series (fromList 0 [n])

shiftRight :: Num a => Series a -> Series a
shiftRight (Series xs) = Series (Stream 0 xs)


-- | Multiplies power series by given number
-- 
-- For following series
--   @a0 + a1 * x + a2 * x^2 + ...@
-- coefficients would be
--   @a0, a1, a2, ...@
--
-- Usage examples:
--
-- >>> coefficients (2 *: (x + x ^ 2 + x ^ 4))
-- [0,2,2,0,2,0,0,0,0,0]
-- >>> coefficients (2 *: ((1 + x)^5))
-- [2,10,20,20,10,2,0,0,0,0]
--
infixl 7 *:
(*:) :: Num a => a -> Series a -> Series a
(*:) n (Series xs) = Series ((*) n <$> xs)

-- | Helper function for producing integer
-- coefficients from generating function
-- (assuming denominator of 1 in all coefficients)
--
-- Usage example:
--
-- >>> gen $ (2 + 3 * x)
-- [2,3,0,0,0,0,0,0,0,0]
--
gen :: Series (Ratio Integer) -> Stream Integer
gen (Series xs) = numerator <$> xs

-- | Returns infinite stream of ones
--
-- First 10 elements:
--
-- >>> ones
-- [1,1,1,1,1,1,1,1,1,1]
--
ones :: Stream Integer
ones = gen (1 / (1 - x)) 

-- | Returns infinite stream of natural numbers (excluding zero)
--
-- First 10 natural numbers:
--
-- >>> nats
-- [1,2,3,4,5,6,7,8,9,10]
--
nats :: Stream Integer
nats = gen (1 / ((1 - x) * (1 - x)))

-- assume f(x) = 1 + 2x + 3x + ...
-- we can notice that (x) * x + ones = f(x) 
-- (x - 1) * f(x) + (1 / (1 - x)) = 0 
-- (x - 1) * f(x) = 1 / (x - 1)
-- f(x) = 1 /(1 - x)^2


-- | Returns infinite stream of fibonacci numbers (starting with zero)
--
-- First 10 fibonacci numbers:
--
-- >>> fibs
-- [0,1,1,2,3,5,8,13,21,34]
--
fibs :: Stream Integer
fibs = gen (x / (1 - x - x*x)) 
