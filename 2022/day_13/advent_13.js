#!/usr/local/bin/node

const fs = require("fs");
const stdinBuffer = fs.readFileSync(0);

const debugMode = process.argv.includes("-d");

const println = function (obj) {
    const msg = typeof obj === 'string' ? obj : JSON.stringify(obj);
    console.log(msg);
}

const debug = function (objFactory) {
    if (!debugMode) return;
    const obj = typeof objFactory === 'function' ? objFactory() : objFactory;
    println(obj);
}

const pairs = stdinBuffer.toString().split("\n\n")
    .filter((it) => !!it)
    .map((pair) => pair.split("\n").map((line) => JSON.parse(line)));

debug(() => `Read ${pairs.length} pairs`);
debug(pairs[0]);

const isInt = (x) => typeof x === 'number';
const wrapIfInt = (x) => isInt(x) ? [x] : x;

let coverage = new Set();
Boolean.prototype.debug = function(n) {
    debug(`returning ${this} at ${n}`);
    debug(coverage.add(n));
    return this.valueOf();
};

function check([a, b]) {
    debug(`Compare ${JSON.stringify(a)} vs ${JSON.stringify(b)}`);
    const minLength = Math.min(a.length, b.length);
    for (let i = 0; i < minLength; i++) {
        debug(`i = ${i}`);
        const [le, ri] = [a[i], b[i]];
        debug(`Compare ${JSON.stringify(le)} vs ${JSON.stringify(ri)}`);
        if (isInt(le) && isInt(ri)) {
            if (parseInt(le) < parseInt(ri)) return true.debug('two ints');
            if (parseInt(le) > parseInt(ri)) return false.debug('two ints');
        } else if (!isInt(le) && !isInt(ri)) {
            let subResult = undefined;
            let j = 0;
            for (; j < Math.min(le.length, ri.length); j++) {
                subResult = check([le[j], ri[j]]);
                if (subResult !== undefined) return subResult.debug('two lists inner');
            }
            println([j, le.length, ri.length]);
            if (le.length < ri.length) return true.debug('two lists outer 1');
            if (le.length > ri.length) return false.debug('two lists outer 2');
        } else {
            const subResult = check([wrapIfInt(le), wrapIfInt(ri)]);
            if (subResult !== undefined) return subResult.debug('uneven');
        }
    }
    if (a.length < b.length) return true.debug('final outer 1');
    if (a.length > b.length) return false.debug('final outer 2');
    return undefined;
}

Array.prototype.sum = function () { return this.reduce((acc, it) => acc + it, 0); };
// pairs.map((it, idx) => `${idx+1}: ${check(it)}`).forEach(debug);
// process.exit(0);
const part1answer = pairs
    .map((it, idx) => {
        debug(`\n== Pair ${idx + 1} ==`);
        const ret = check(it);
        debug(`Result: ${ret}`);
        return ret ? idx + 1 : 0;
    })
    .map((it) => { debug(it); return it; })
    .sum();

// 5517 is too high
// 5480 is too high
println(`Part 1: ${part1answer}`);
console.log(coverage);

