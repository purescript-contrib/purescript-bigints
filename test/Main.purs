module Test.Main where

import Prelude hiding (not)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, Error, message)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (filter, range)
import Data.Array.NonEmpty as NEA
import Data.BigInt (BigInt, abs, and, digitsInBase, even, fromBase, fromInt, fromNumber, fromString, not, odd, or, pow, prime, shl, shr, toBase, toBase', toNonEmptyString, toNumber, toString, xor)
import Data.Either (Either(..), either, fromLeft, fromRight, isLeft)
import Data.Foldable (fold)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.String.NonEmpty (unsafeFromString)
import Data.String.NonEmpty as NES
import Global (infinity, nan)
import Partial.Unsafe (unsafePartial)
import Test.Assert (ASSERT, assert)
import Test.QuickCheck (QC, quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt, elements, resize)
import Test.QuickCheck.Laws.Data as Data
import Type.Proxy (Proxy(..))

-- | Newtype with an Arbitrary instance that generates only small integers
newtype SmallInt = SmallInt Int

instance arbitrarySmallInt :: Arbitrary SmallInt where
  arbitrary = SmallInt <$> chooseInt (-5) 5

runSmallInt :: SmallInt -> Int
runSmallInt (SmallInt n) = n

-- | Arbitrary instance for BigInt
newtype TestBigInt = TestBigInt BigInt
derive newtype instance eqTestBigInt :: Eq TestBigInt
derive newtype instance ordTestBigInt :: Ord TestBigInt
derive newtype instance semiringTestBigInt :: Semiring TestBigInt
derive newtype instance ringTestBigInt :: Ring TestBigInt
derive newtype instance commutativeRingTestBigInt :: CommutativeRing TestBigInt
derive newtype instance euclideanRingTestBigInt :: EuclideanRing TestBigInt

instance arbitraryBigInt :: Arbitrary TestBigInt where
  arbitrary = do
    n <- (either (const zero) id <<< fromString) <$> digitString
    op <- elements (id :| [negate])
    pure (TestBigInt (op n))
    where digits :: Gen Int
          digits = chooseInt 0 9
          digitString :: Gen String
          digitString = (fold <<< map show) <$> (resize 50 $ arrayOf digits)

-- | Convert SmallInt to BigInt
fromSmallInt :: SmallInt -> Either Error BigInt
fromSmallInt = fromInt <<< runSmallInt

-- | Test if a binary relation holds before and after converting to BigInt.
testBinary :: forall eff. (BigInt -> BigInt -> BigInt)
           -> (Int -> Int -> Int)
           -> QC eff Unit
testBinary f g =
  quickCheck \x y ->
    case f <$> fromInt x <*> fromInt y, fromInt (x `g` y) of
      Right fResult, Right gResult -> fResult == gResult
      _            , _             -> false

main :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT, random :: RANDOM, exception :: EXCEPTION | eff) Unit
main = unsafePartial do
  log "Simple arithmetic operations and conversions from Int"
  let two = one + one
  let three = two + one
  let four = three + one
  assert $ fromRight (fromInt 3) == three
  assert $ two * two == four
  assert $ two * three * (three + four) == fromRight (fromInt 42)
  assert $ two - three == fromRight (fromInt (-1))

  log "Parsing strings"
  assert $ fromRight (fromString "2") == two
  assert $ isLeft $ fromString "a"
  assert $ isLeft $ fromString "2.1"
  assert $ fromRight (fromString "123456789") == fromRight (fromInt 123456789)
  assert $ fromRight (fromString "1e7") == fromRight (fromInt 10000000)
  quickCheck $ \(TestBigInt a) -> (fromRight <<< fromString <<< toString) a == a

  log "Parsing strings with a different base"
  assert $ fromRight (fromBase 2 "100") == four
  assert $ fromRight (fromBase 16 "ff") == fromRight (fromString "255")

  log "Rendering bigints as strings with a different base"
  assert $ toBase 2 four == "100"
  assert $ fromRight (toBase 16 <$> fromString "255") == "ff"
  assert $ toString (fromRight (fromInt 12345)) == "12345"

  log "Converting bigints to arrays with a different base"
  assert $ NEA.toArray (digitsInBase 2 four).value == [1, 0, 0]
  assert $ fromRight (NEA.toArray <<< _.value <<< digitsInBase 16 <$>
           fromString "255") == [15, 15]
  assert $ NEA.toArray (digitsInBase 10 $ fromRight (fromInt 12345)).value
           == [1, 2, 3, 4, 5]

  assert $ toBase' 2 four == unsafePartial unsafeFromString "100"
  assert $ Just (fromRight (toBase' 16 <$> fromString "255")) == NES.fromString "ff"
  assert $ toNonEmptyString (fromRight (fromInt 12345))
           == unsafePartial unsafeFromString "12345"

  log "Converting from Number to BigInt"
  assert $ fromRight (fromNumber 0.0) == zero
  assert $ fromRight (fromNumber 3.4) == three
  assert $ fromRight (fromNumber (-3.9)) == -three
  assert $ fromRight (fromNumber 1.0e7) == fromRight (fromInt 10000000)
  assert $ fromRight (fromNumber 1.0e47) == fromRight (fromString "1e47")
  quickCheck (\x -> fromRight (fromInt x) == fromRight (fromNumber (Int.toNumber x)))

  log "Conversions between String, Int and BigInt should not loose precision"
  quickCheck (\n -> fromRight (fromString (show n)) == fromRight (fromInt n))
  quickCheck (\n -> Int.toNumber n == toNumber (fromRight (fromInt n)))

  log "Binary relations between integers should hold before and after converting to BigInt"
  testBinary (+) (+)
  testBinary (-) (-)
  testBinary (/) (/)
  testBinary mod mod
  testBinary div div

  -- To test the multiplication, we need to make sure that Int does not overflow
  quickCheck (\x y -> fromRight (fromSmallInt x) * fromRight (fromSmallInt y) == fromRight (fromInt (runSmallInt x * runSmallInt y)))

  log "It should perform multiplications which would lead to imprecise results using Number"
  assert $ fromRight (fromInt 333190782 * fromInt 1103515245) == fromRight (fromString "367681107430471590")

  log "compare, (==), even, odd should be the same before and after converting to BigInt"
  quickCheck (\x y -> compare x y == compare (fromRight (fromInt x)) (fromRight (fromInt y)))
  quickCheck (\x y -> (fromRight (fromSmallInt x) == fromRight (fromSmallInt y)) == (runSmallInt x == runSmallInt y))
  quickCheck (\x -> Int.even x == even (fromRight (fromInt x)))
  quickCheck (\x -> Int.odd x == odd (fromRight (fromInt x)))

  log "pow should perform integer exponentiation and yield 0 for negative exponents"
  assert $ three `pow` four == fromRight (fromInt 81)
  assert $ three `pow` -two == zero
  assert $ three `pow` zero == one
  assert $ zero `pow` zero == one

  log "Prime numbers"
  assert $ filter (prime <<< fromRight <<< fromInt) (range 2 20) == [2, 3, 5, 7, 11, 13, 17, 19]

  log "Absolute value"
  quickCheck $ \(TestBigInt x) -> abs x == if x > zero then x else (-x)

  log "Logic"
  assert $ (not <<< not) one == one
  assert $ or one three == three
  assert $ xor one three == two
  assert $ and one three == one

  log "Shifting"
  assert $ shl two one == four
  assert $ shr two one == one

  let prxBigInt = Proxy ∷ Proxy TestBigInt
  Data.checkEq prxBigInt
  Data.checkOrd prxBigInt
  Data.checkSemiring prxBigInt
  Data.checkRing prxBigInt
  -- Data.checkEuclideanRing prxBigInt
  Data.checkCommutativeRing prxBigInt

  log "Infinity and NaN"
  assert $ message (fromLeft (fromNumber infinity)) == "Invalid integer: Infinity"
  assert $ message (fromLeft (fromNumber (-infinity))) == "Invalid integer: Infinity"
  assert $ message (fromLeft (fromNumber nan)) == "Invalid integer: NaN"
