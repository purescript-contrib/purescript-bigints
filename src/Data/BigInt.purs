-- | This module defines a `BigInt` data type for arbitrary length integers.
module Data.BigInt
  ( BigInt(..)
  , BaseDigits
  , fromString
  , fromBase
  , fromInt
  , fromNumber
  , fromTLInt
  , toString
  , toNonEmptyString
  , toBase
  , toBase'
  , digitsInBase
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
  , quot
  , rem
  , toInt
  , toNumber
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Int (floor)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromJust)
import Data.Reflectable (class Reflectable, reflectType)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Partial.Unsafe (unsafePartial)
import Prim.Int (class ToString)
import Type.Proxy (Proxy(..))

-- | An arbitrary length integer.
foreign import data BigInt :: Type

type BaseDigits =
  { value :: NonEmptyArray Int
  , isNegative :: Boolean
  }

-- | FFI wrapper to parse a String in a given base representation into a BigInt.
foreign import fromBaseImpl
  :: forall a
   . (a -> Maybe a)
  -> Maybe a
  -> Int
  -> String
  -> Maybe BigInt

-- | Convert an integer to a BigInt.
foreign import fromInt :: Int -> BigInt

toInt :: BigInt -> Maybe Int
toInt = toNumber >>> Int.fromNumber

-- | FFI wrapper to parse a Number into a BigInt.
foreign import fromNumberImpl
  :: forall a
   . (a -> Maybe a)
  -> Maybe a
  -> Number
  -> Maybe BigInt

-- | Convert a Number to a BigInt. The fractional part is truncated.
fromNumber :: Number -> Maybe BigInt
fromNumber = fromNumberImpl Just Nothing

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

-- Note: this function should not be exported!
-- It's only safe if used with type-level integers.
foreign import fromTypeLevelInt :: String -> BigInt

-- | Converts a type-level integer into a `BigInt`:
-- | ```
-- | import Type.Proxy (Proxy(..))
-- | foo = fromTLInt (Proxy :: Proxy 857981209301293808359384092830482)
-- | ```
fromTLInt :: forall i sym. ToString i sym => Reflectable sym String => Proxy i -> BigInt
fromTLInt _ = fromTypeLevelInt (reflectType (Proxy :: Proxy sym))

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
fromBase = fromBaseImpl Just Nothing

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
toString :: BigInt -> String
toString = toBase 10

-- | A decimal representation of the `BigInt` as a `NonEmptyString`.
toNonEmptyString :: BigInt -> NonEmptyString
toNonEmptyString = toBase' 10

-- | A base N representation of the `BigInt` as an array of digits.
foreign import digitsInBase :: Int -> BigInt -> BaseDigits

-- | A base N representation of the `BigInt` as a `String`.
foreign import toBase :: Int -> BigInt -> String

-- | A base N representation of the `BigInt` as a `NonEmptyString`.
toBase' :: Int -> BigInt -> NonEmptyString
toBase' i bi = unsafePartial fromJust $ NES.fromString $ toBase i bi

instance showBigInt :: Show BigInt where
  show x = "(fromString \"" <> toString x <> "\")"

foreign import biAdd :: BigInt -> BigInt -> BigInt
foreign import biMul :: BigInt -> BigInt -> BigInt

instance semiringBigInt :: Semiring BigInt where
  add = biAdd
  zero = fromInt 0
  mul = biMul
  one = fromInt 1

foreign import biSub :: BigInt -> BigInt -> BigInt

instance ringBigInt :: Ring BigInt where
  sub = biSub

foreign import biDiv :: BigInt -> BigInt -> BigInt
foreign import biMod :: BigInt -> BigInt -> BigInt

instance commutativeRingBigInt :: CommutativeRing BigInt

instance euclideanRingBigInt :: EuclideanRing BigInt where
  div x y = (x - x `mod` y) `biDiv` y

  mod x y = ((x `biMod` yy) + yy) `biMod` yy
    where
    yy = abs y

  degree = floor <<< toNumber <<< abs

-- | Truncating integer division
quot :: BigInt -> BigInt -> BigInt
quot = biDiv

-- | The remainder after truncating integer division
rem :: BigInt -> BigInt -> BigInt
rem = biMod
