// module Data.BigInt

var bigInt = require("big-integer");

exports.fromBaseImpl = function(just) {
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

function truncate(n) {
  if (n > 0) return Math.floor(n);
  return Math.ceil(n);
}

exports.fromNumberImpl = function(just) {
  return function(nothing) {
      return function(n) {
        try {
          var x = bigInt(truncate(n));
          return just(x);
        } catch (err) {
          return nothing;
        }
      };
  };
};

exports.fromInt = function(n) {
  return bigInt(n);
};

exports.toBase = function(base) {
  return function (x) {
    return x.toString(base);
  };
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

exports.digitsInBase = function(radix) {
  return function(x) {
    return x.toArray(radix);
  };
};
