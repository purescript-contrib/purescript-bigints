// module Data.BigInt

var bigInt = require("big-integer");

exports["fromBase'"] = function(just) {
  return function(nothing) {
    return function(b) {
      return function(s) {
        try {
          var x = bigInt(s, b);
          return just(x);
        } catch (err) {
          return nothing;
        }
      };
    };
  };
};

exports.fromInt = bigInt;

exports.toString = function(x) {
  return x.toString();
};

exports.toNumber = function(x) {
  return x.toJSNumber();
};

exports.biAdd = function(x) {
  return function(y) {
    return x.add(y);
  };
};

exports.biMul = function(x) {
  return function(y) {
    return x.multiply(y);
  };
};

exports.biSub = function(x) {
  return function(y) {
    return x.minus(y);
  };
};

exports.biMod = function(x) {
  return function(y) {
    return x.mod(y);
  };
};

exports.biDiv = function(x) {
  return function(y) {
    return x.divide(y);
  };
};

exports.biEquals = function(x) {
  return function(y) {
    return x.equals(y);
  };
};

exports.biCompare = function(x) {
  return function(y) {
    return x.compare(y);
  };
};

exports.abs = function(x) {
  return x.abs();
};

exports.even = function(x) {
  return x.isEven();
};

exports.odd = function(x) {
  return x.isOdd();
};

exports.prime = function(x) {
  return x.isPrime();
};

exports.pow = function(x) {
  return function(y) {
    return x.pow(y);
  };
};

exports.not = function(x) {
  return x.not();
  };

exports.or = function(x) {
  return function(y) {
    return x.or(y);
  };
};

exports.xor = function(x) {
  return function(y) {
    return x.xor(y);
  };
};

exports.and = function(x) {
  return function(y) {
    return x.and(y);
  };
};

exports.shl = function(x) {
  return function(n) {
    return x.shiftLeft(n);
  };
};

exports.shr = function(x) {
  return function(n) {
    return x.shiftRight(n);
  };
};

