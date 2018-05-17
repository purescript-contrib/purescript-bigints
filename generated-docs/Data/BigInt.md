## Module Data.BigInt

This module defines a `BigInt` data type for arbitrary length integers.

#### `BigInt`

``` purescript
data BigInt :: Type
```

An arbitrary length integer.

##### Instances
``` purescript
Eq BigInt
Ord BigInt
Show BigInt
Semiring BigInt
Ring BigInt
CommutativeRing BigInt
EuclideanRing BigInt
```

#### `BaseDigits`

``` purescript
type BaseDigits = { value :: NonEmptyArray Int, isNegative :: Boolean }
```

#### `fromString`

``` purescript
fromString :: String -> Maybe BigInt
```

Parse a string into a `BigInt`, assuming a decimal representation. Returns
`Nothing` if the parse fails.

Examples:
```purescript
fromString "42"
fromString "857981209301293808359384092830482"
fromString "1e100"
```

#### `fromBase`

``` purescript
fromBase :: Int -> String -> Maybe BigInt
```

Parse a string into a `BigInt`, assuming a representation in the given base.
The letters "a-z" and "A-Z" will be interpreted as the digits `10` to
`36`. Returns `Nothing` if the parse fails.

```purescript
fromBase 2 "100" == fromString "4"
fromBase 16 "ff" == fromString "255"
```

#### `fromInt`

``` purescript
fromInt :: Int -> BigInt
```

Convert an integer to a BigInt.

#### `fromNumber`

``` purescript
fromNumber :: Number -> Maybe BigInt
```

Convert a Number to a BigInt. The fractional part is truncated.

#### `toString`

``` purescript
toString :: BigInt -> String
```

A decimal representation of the `BigInt` as a `String`.

#### `toNonEmptyString`

``` purescript
toNonEmptyString :: BigInt -> NonEmptyString
```

A decimal representation of the `BigInt` as a `NonEmptyString`.

#### `toBase`

``` purescript
toBase :: Int -> BigInt -> String
```

A base N representation of the `BigInt` as a `String`.

#### `toBase'`

``` purescript
toBase' :: Int -> BigInt -> NonEmptyString
```

A base N representation of the `BigInt` as a `NonEmptyString`.

#### `digitsInBase`

``` purescript
digitsInBase :: Int -> BigInt -> BaseDigits
```

A base N representation of the `BigInt` as an array of digits.

#### `abs`

``` purescript
abs :: BigInt -> BigInt
```

The absolute value.

#### `even`

``` purescript
even :: BigInt -> Boolean
```

Returns `true` if the number is even, `false` otherwise.

#### `odd`

``` purescript
odd :: BigInt -> Boolean
```

Returns `true` if the number is odd, `false` otherwise.

#### `prime`

``` purescript
prime :: BigInt -> Boolean
```

Returns `true` if the number is prime, `false` otherwise.

#### `pow`

``` purescript
pow :: BigInt -> BigInt -> BigInt
```

Exponentiation for `BigInt`. If the exponent is less than 0, `pow`
returns 0. Also, `pow zero zero == one`.

#### `not`

``` purescript
not :: BigInt -> BigInt
```

Invert the bits.

#### `or`

``` purescript
or :: BigInt -> BigInt -> BigInt
```

or the bits.

#### `xor`

``` purescript
xor :: BigInt -> BigInt -> BigInt
```

Exlusive or the bits.

#### `and`

``` purescript
and :: BigInt -> BigInt -> BigInt
```

and the bits.

#### `shl`

``` purescript
shl :: BigInt -> Number -> BigInt
```

shift the bits left and zero fill.

#### `shr`

``` purescript
shr :: BigInt -> Number -> BigInt
```

Shift the bits right and maintain pos/neg.

#### `toNumber`

``` purescript
toNumber :: BigInt -> Number
```

Converts a BigInt to a Number. Loses precision for numbers which are too
large.


