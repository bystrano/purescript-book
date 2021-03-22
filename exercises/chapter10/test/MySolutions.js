"use strict";

exports.volumeFn = function (a, b, c) {
  return a * b * c;
};

exports.volumeArrow = (a) => (b) => (c) => a * b * c;

exports.cumulativeSumsComplex = (arr) => {
  let sum = { real: 0, imag: 0 };
  let sums = [];

  arr.forEach((x) => {
    sum = {
      real: sum.real + x.real,
      imag: sum.imag + x.imag,
    };
    sums.push(sum);
  });

  return sums;
};

exports.quadraticRootsImpl = (pair) => (a) => (b) => (c) => {
  const discriminant = b * b - 4 * a * c;
  if (discriminant >= 0) {
    return pair({
      real: (-b + Math.sqrt(discriminant)) / (2 * a),
      imag: 0,
    })({
      real: (-b - Math.sqrt(discriminant)) / (2 * a),
      imag: 0,
    });
  } else {
    return pair({
      real: -b / (2 * a),
      imag: Math.sqrt(-discriminant) / (2 * a),
    })({
      real: -b / (2 * a),
      imag: -Math.sqrt(-discriminant) / (2 * a),
    });
  }
};
