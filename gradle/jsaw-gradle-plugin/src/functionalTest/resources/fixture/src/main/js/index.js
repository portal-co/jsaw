import { triple } from './lib.js';

export function run(a) { return triple(a) + 1; }

export function count(n) {
    let f = function(k, acc) { if (k <= 0) { return acc; } return f(k - 1, acc + k); };
    return f(n, 0);
}
