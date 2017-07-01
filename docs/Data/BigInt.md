## Module Data.BigInt

This module defines a `BigInt` data type for arbitrary length integers.

#### `BigInt`

``` purescript
data BigInt :: *
```

An arbitrary length integer.

##### Instances
``` purescript
Eq BigInt
Ord BigInt
Show BigInt
Semiring BigInt
Ring BigInt
ModuloSemiring BigInt
```

#### `fromInt`

``` purescript
fromInt :: Int -> BigInt
```

Convert an integer to a BigInt.

#### `toNumber`

``` purescript
toNumber :: BigInt -> Number
```

Converts a BigInt to a Number. Loses precision for numbers which are too
large.

#### `pow`

``` purescript
pow :: BigInt -> BigInt -> BigInt
```

Exponentiation for `BigInt`. If the exponent is less than 0, `pow`
returns 0. Also, `pow zero zero == one`.

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

#### `not`

``` purescript
not :: BigInt -> BigInt
```

Returns a bit inverted number.

#### `or`

``` purescript
or :: BigInt -> BigInt -> BigInt
```

Returns a bit or'ed number.

#### `xor`

``` purescript
xor :: BigInt -> BigInt -> BigInt
```

Returns a bit xor'ed numnber.

#### `and`

``` purescript
and :: BigInt -> BigInt -> BigInt
```

Returns a bit and'ed number.

#### `shl`

``` purescript
shl :: BigInt -> Number -> BigInt
```

Shift the first number to the right by the second number of bits and return the result. Shifts in ones if the first number is negative.

#### `shr`

``` purescript
shr :: BigInt -> Number -> BigInt
```

Shift the first number to the left by the second number of bits and return the result.

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

#### `toString`

``` purescript
toString :: BigInt -> String
```

A decimal representation of the `BigInt` as a `String`.


