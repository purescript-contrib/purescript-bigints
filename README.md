# purescript-bigints

A library for calculations with arbitrary length integers.
This is a simple wrapper around [BigInteger.js](https://github.com/peterolson/BigInteger.js)
by [Peter Olson](https://github.com/peterolson).

## Example

```purescript
x = fromInt 42
y = (fromJust <<< fromBase 16) "fe45aab12"
mersenne10 = (fromInt 2) `pow` (fromInt 89) - one

> x `pow` x
fromString "150130937545296572356771972164254457814047970568738777235893533016064"

> toNumber (y * mersenne10)
4.2248205181941055e+37

> prime mersenne10
true
```

## Module documentation

- [Data.BigInt](docs/Data/BigInt.md)

## Installation

```
npm install
bower install
gulp
```
