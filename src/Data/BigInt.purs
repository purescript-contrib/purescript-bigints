-- | This module defines a `BigInt` data type for arbitrary length integers.
module Data.BigInt
  ( BigInt(..)
  , fromString
  , fromBase
  , fromInt
  , toString
  , abs
  , even
  , odd
  , prime
  , pow
  , not
  , or
  , xor
  , and
  , shl
  , shr
  , toNumber
  ) where

import Prelude

import Data.Maybe (Maybe(..))

-- | An arbitrary length integer.
foreign import data BigInt :: Type

-- | FFI wrapper to parse a String in a given base representation into a BigInt.
foreign import fromBase' :: forall a. (a -> Maybe a)
                         -> Maybe a
                         -> Int
                         -> String
                         -> Maybe BigInt

-- | Convert an integer to a BigInt.
foreign import fromInt :: Int -> BigInt

-- | Converts a BigInt to a Number. Loses precision for numbers which are too
-- | large.
foreign import toNumber :: BigInt -> Number

-- | Exponentiation for `BigInt`. If the exponent is less than 0, `pow`
-- | returns 0. Also, `pow zero zero == one`.
foreign import pow :: BigInt -> BigInt -> BigInt

-- | The absolute value.
foreign import abs :: BigInt -> BigInt

-- | Returns `true` if the number is even, `false` otherwise.
foreign import even :: BigInt -> Boolean

-- | Returns `true` if the number is odd, `false` otherwise.
foreign import odd :: BigInt -> Boolean

-- | Returns `true` if the number is prime, `false` otherwise.
foreign import prime :: BigInt -> Boolean

-- | Invert the bits.
foreign import not :: BigInt -> BigInt

-- | or the bits.
foreign import or :: BigInt -> BigInt -> BigInt

-- | Exlusive or the bits.
foreign import xor :: BigInt -> BigInt -> BigInt

-- | and the bits.
foreign import and :: BigInt -> BigInt -> BigInt

-- | shift the bits left and zero fill.
foreign import shl :: BigInt -> Number -> BigInt

-- | Shift the bits right and maintain pos/neg.
foreign import shr :: BigInt -> Number -> BigInt

-- | Parse a string into a `BigInt`, assuming a decimal representation. Returns
-- | `Nothing` if the parse fails.
-- |
-- | Examples:
-- | ```purescript
-- | fromString "42"
-- | fromString "857981209301293808359384092830482"
-- | fromString "1e100"
-- | ```
fromString :: String -> Maybe BigInt
fromString = fromBase 10

-- | Parse a string into a `BigInt`, assuming a representation in the given base.
-- | The letters "a-z" and "A-Z" will be interpreted as the digits `10` to
-- | `36`. Returns `Nothing` if the parse fails.
-- |
-- | ```purescript
-- | fromBase 2 "100" == fromString "4"
-- | fromBase 16 "ff" == fromString "255"
-- | ```
fromBase :: Int -> String -> Maybe BigInt
fromBase = fromBase' Just Nothing

foreign import biEquals :: BigInt -> BigInt -> Boolean

instance eqBigInt :: Eq BigInt where
  eq = biEquals

foreign import biCompare :: BigInt -> BigInt -> Int

instance ordBigInt :: Ord BigInt where
  compare x y = case biCompare x y of
                  1 -> GT
                  0 -> EQ
                  _ -> LT

-- | A decimal representation of the `BigInt` as a `String`.
foreign import toString :: BigInt -> String

instance showBigInt :: Show BigInt where
  show x = "fromString \"" <> toString x <> "\""

foreign import biAdd :: BigInt -> BigInt -> BigInt
foreign import biMul :: BigInt -> BigInt -> BigInt

instance semiringBigInt :: Semiring BigInt where
  add  = biAdd
  zero = fromInt 0
  mul  = biMul
  one  = fromInt 1

foreign import biSub :: BigInt -> BigInt -> BigInt

instance ringBigInt :: Ring BigInt where
  sub = biSub

foreign import biDiv :: BigInt -> BigInt -> BigInt
foreign import biMod :: BigInt -> BigInt -> BigInt

instance commutativeRingBigInt :: CommutativeRing BigInt

instance euclideanRingBigInt :: EuclideanRing BigInt where
  div = biDiv
  mod = biMod
  degree = degree <<< toNumber
