"use strict";

exports.volumeFn = function (a, b, c) {
  return a * b * c;
};

exports.volumeArrow = (a) => (b) => (c) => a * b * c;

exports.cumulativeSumsComplex = (arr) => {
  let sum = { real: 0, imag: 0 };
  let sums = [];

  arr.forEach(x => {
    sum = {
      real: sum.real + x.real,
      imag: sum.imag + x.imag
    };
    sums.push(sum);
  });

  return sums;
}
