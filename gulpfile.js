/* jshint node: true */

"use strict";

var gulp = require("gulp");
var purescript = require("gulp-purescript");
var run = require("gulp-run");
var rimraf = require("rimraf");

var sources = [
  "src/**/*.purs",
  "test/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs"
];

var foreigns = [
  "src/**/*.js",
  "bower_components/purescript-*/src/**/*.js"
];

gulp.task("clean-docs", function (cb) {
  rimraf("docs", cb);
});

gulp.task("clean-output", function (cb) {
  rimraf("output", cb);
});

gulp.task("clean", ["clean-docs", "clean-output"]);

gulp.task("make", function() {
  return purescript.psc({
    src: sources,
    ffi: foreigns,
    output: "output"
  });
});

gulp.task("docs", ["clean-docs"], function () {
  return purescript.pscDocs({
    src: sources,
    docgen: {
      "Data.BigInt": "docs/Data/BigInt.md"
    }
  });
});

gulp.task("test", ["make"], function() {
  return purescript.pscBundle({
    src: ['output/**/*.js', 'node_modules/big-integert/BigInteger.min.js'],
    main: 'Test.Main'
  })
  .pipe(run("node"));
});

gulp.task("default", ["make", "docs", "test"]);
