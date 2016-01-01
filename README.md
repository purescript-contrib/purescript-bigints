# purescript-bigints

[![Latest release](http://img.shields.io/bower/v/purescript-bigints.svg)](https://github.com/sharkdp/purescript-bigints/releases)
[![Build Status](https://api.travis-ci.org/sharkdp/purescript-bigints.svg?branch=master)](https://travis-ci.org/sharkdp/purescript-bigints)

A library for calculations with arbitrary length integers.
This is a simple wrapper around [BigInteger.js](https://github.com/peterolson/BigInteger.js)
by [Peter Olson](https://github.com/peterolson).


## Module documentation

- [Published on Pursuit](http://pursuit.purescript.org/packages/purescript-bigints/)

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

## Installation and usage
You can install this package via Bower. You will also need [BigInteger.js](https://github.com/peterolson/BigInteger.js), which can be installed via `npm`:
```
bower install purescript-bigints
npm install big-integer
```
For the browser, remember to bundle `BigInteger.min.js` with your code.

## Development
```
bower install
npm install
```
Then, use `pulp` to build, run tests and generate the documentation.
