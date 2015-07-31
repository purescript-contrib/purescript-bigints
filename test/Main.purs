module Test.Main where

import Control.Monad.Eff.Console (log)
import Data.BigInt
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Prelude
import Test.Assert
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen (chooseInt)
import qualified Data.Int as Int

-- | Newtype with an Arbitrary instance that generates only small integers
newtype SmallInt = SmallInt Int

instance arbitrarySmallInt :: Arbitrary SmallInt where
  arbitrary = SmallInt <$> chooseInt (-5) 5

runSmallInt :: SmallInt -> Int
runSmallInt (SmallInt n) = n

-- | Convert SmallInt to BigInt
fromSmallInt :: SmallInt -> BigInt
fromSmallInt = fromInt <<< runSmallInt

-- | Test if a binary relation holds before and after converting to BigInt.
testBinary :: (BigInt -> BigInt -> BigInt)
           -> (Int -> Int -> Int)
           -> _
testBinary f g = quickCheck (\x y -> (fromInt x) `f` (fromInt y) == fromInt (x `g` y))

main = do
  log "Simple arithmetic operations and conversions from Int"
  let two = one + one
  let three = two + one
  let four = three + one
  assert $ fromInt 3 == three
  assert $ two * two == four
  assert $ two * three * (three + four) == fromInt 42
  assert $ two - three == fromInt (-1)

  log "Parsing strings"
  assert $ fromString "2" == Just two
  assert $ fromString "a" == Nothing
  assert $ fromString "2.1" == Nothing
  assert $ fromString "123456789" == Just (fromInt 123456789)
  assert $ fromString "1e7" == Just (fromInt 10000000)

  log "Parsing strings with a different base"
  assert $ fromBase 2 "100" == Just four
  assert $ fromBase 16 "ff" == fromString "255"

  log "Conversions between String, Int and BigInt should not loose precision"
  quickCheck (\n -> fromString (show n) == Just (fromInt n))
  quickCheck (\n -> Int.toNumber n == toNumber (fromInt n))

  log "Binary relations between integers should hold before and after converting to BigInt"
  testBinary (+) (+)
  testBinary (-) (-)
  testBinary (/) (/)
  testBinary mod mod
  testBinary div div

  log "It should perform multiplications which would lead to imprecise results using Number"
  assert $ Just (fromInt 333190782 * fromInt 1103515245) == fromString "367681107430471590"

  -- To check the multiplication, we need to make sure that the Int does not overflow
  quickCheck (\x y -> fromSmallInt x * fromSmallInt y == fromInt (runSmallInt x * runSmallInt y))

  log "compare and (==) should be the same before and after converting to BigInt"
  quickCheck (\x y -> compare x y == compare (fromInt x) (fromInt y))
  quickCheck (\x y -> (fromSmallInt x == fromSmallInt y) == (runSmallInt x == runSmallInt y))

  log "pow should perform integer exponentiation and yield 0 for negative exponents"
  assert $ three `pow` four == fromInt 81
  assert $ three `pow` -two == zero
  assert $ three `pow` zero == one
  assert $ zero `pow` zero == one
