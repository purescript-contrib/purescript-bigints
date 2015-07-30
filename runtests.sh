#!/bin/bash

set -e

psc --ffi 'bower_components/purescript-*/src/**/*.js' \
    --ffi 'src/**/*.js' \
    'bower_components/purescript-*/src/**/*.purs' \
    'src/**/*.purs' \
    'test/**/*.purs'

rm -rf docs

psc-docs 'src/**/*.purs' \
    'bower_components/purescript-*/src/**/*.purs' \
    --docgen 'Data.BigInt:docs/Data/BigInt.md'

psc-bundle 'output/**/*.js' \
    'node_module/big-integer/BigInteger.min.js' \
    --module 'Test.Main' \
    --main   'Test.Main' \
    --output 'test.js'

node test.js

rm test.js
