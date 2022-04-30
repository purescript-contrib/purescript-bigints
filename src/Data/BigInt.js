// module Data.BigInt

import bigInt from "big-integer";

export function fromTypeLevelInt(str) {
  return bigInt(str, 10);
}

export function fromBaseImpl(just) {
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
}

function truncate(n) {
  if (n > 0) return Math.floor(n);
  return Math.ceil(n);
}

export function fromNumberImpl(just) {
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
}

export function fromInt(n) {
  return bigInt(n);
}

export function toBase(base) {
  return function (x) {
    return x.toString(base);
  };
}

export function toNumber(x) {
  return x.toJSNumber();
}

export function biAdd(x) {
  return function(y) {
    return x.add(y);
  };
}

export function biMul(x) {
  return function(y) {
    return x.multiply(y);
  };
}

export function biSub(x) {
  return function(y) {
    return x.minus(y);
  };
}

export function biMod(x) {
  return function(y) {
    return x.mod(y);
  };
}

export function biDiv(x) {
  return function(y) {
    return x.divide(y);
  };
}

export function biEquals(x) {
  return function(y) {
    return x.equals(y);
  };
}

export function biCompare(x) {
  return function(y) {
    return x.compare(y);
  };
}

export function abs(x) {
  return x.abs();
}

export function even(x) {
  return x.isEven();
}

export function odd(x) {
  return x.isOdd();
}

export function prime(x) {
  return x.isPrime();
}

export function pow(x) {
  return function(y) {
    return x.pow(y);
  };
}

export function not(x) {
  return x.not();
  }

export function or(x) {
  return function(y) {
    return x.or(y);
  };
}

export function xor(x) {
  return function(y) {
    return x.xor(y);
  };
}

export function and(x) {
  return function(y) {
    return x.and(y);
  };
}

export function shl(x) {
  return function(n) {
    return x.shiftLeft(n);
  };
}

export function shr(x) {
  return function(n) {
    return x.shiftRight(n);
  };
}

export function digitsInBase(radix) {
  return function(x) {
    return x.toArray(radix);
  };
}
