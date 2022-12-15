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

function compare(a, b) {
    debug(`Compare ${JSON.stringify(a)} vs ${JSON.stringify(b)}`);

    if (isInt(a) && isInt(b)) return (a === b) ? 0 : (a < b ? 1 : -1);
    if (isInt(a) !== isInt(b)) return compare(wrapIfInt(a), wrapIfInt(b));

    // from now on a and b must be lists

    for (let j = 0; j < Math.min(a.length, b.length); j++) {
        const [le, ri] = [a[j], b[j]];
        debug(`Compare ${JSON.stringify(le)} vs ${JSON.stringify(ri)}`);
        if (isInt(le) && isInt(ri)) {
            const cmp = compare(le, ri);
            if (cmp !== 0) return cmp;
        } else if (!isInt(le) && !isInt(ri)) {
            for (let k = 0; k < Math.min(le.length, ri.length); k++) {
                const cmp = compare(le[k], ri[k]);
                if (cmp !== 0) return cmp;
            }
            const cmp = compare(le.length, ri.length);
            if (cmp !== 0) return cmp;
        } else {
            const cmp = compare(wrapIfInt(le), wrapIfInt(ri));
            if (cmp !== 0) return cmp;
        }
    }

    return compare(a.length, b.length);
}

const checkInt = (a, b) => (a === b) ? undefined : (a < b);

/** Does not give the correct answer on the input, idk why */
function check([a, b]) {
    debug(`Compare ${JSON.stringify(a)} vs ${JSON.stringify(b)}`);

    if (isInt(a) !== isInt(b)) return check([wrapIfInt(a), wrapIfInt(b)]);
    if (isInt(a) && isInt(b)) return checkInt(a, b);

    for (let i = 0; i < Math.min(a.length, b.length); i++) {
        debug(`i = ${i}`);
        const [le, ri] = [a[i], b[i]];
        debug(`Compare ${JSON.stringify(le)} vs ${JSON.stringify(ri)}`);
        if (isInt(le) && isInt(ri)) {
            if (parseInt(le) < parseInt(ri)) return true;
            if (parseInt(le) > parseInt(ri)) return false;
        } else if (!isInt(le) && !isInt(ri)) {
            for (let j = 0; j < Math.min(le.length, ri.length); j++) {
                const subResult = check([le[j], ri[j]]);
                if (subResult !== undefined) return subResult;
            }
            if (le.length < ri.length) return true;
            if (le.length > ri.length) return false;
        } else {
            const subResult = check([wrapIfInt(le), wrapIfInt(ri)]);
            if (subResult !== undefined) return subResult;
        }
    }
    if (a.length < b.length) return true;
    if (a.length > b.length) return false;

    return undefined;
}

// const results = pairs.map((pair, idx) => [idx, check(pair) === true, compare(...pair) === 1]);
// results.filter(([idx, r1, r2]) => r1 !== r2).forEach(println);
// const gap = results.filter(([idx, r1, r2]) => r1 !== r2).reduce((acc, it) => acc + it[0] + 1, 0);
// println(`Gap: ${gap}`);
// 
// for (let w = 0; w < 5; w++) println('\n');
// debug('CHECK');
// check(pairs[35]);
// for (let w = 0; w < 5; w++) println('\n');
// debug('COMPARE');
// compare(...pairs[35]);
// 
// process.exit(0);

const useFunc = check; // check or compare

const part1answer = pairs
    .map((it, idx) => {
        debug(`\n== Pair ${idx + 1} ==`);
        const cmp = useFunc === compare ? compare(...it) : check(it);
        debug(`Result: ${cmp}`);
        return cmp === (useFunc === compare ? 1 : true) ? idx + 1 : 0;
    })
    .map((it) => { it && debug(it); return it; })
    .reduce((acc, it) => acc + it, 0);

println(`Part 1: ${part1answer}`);

const items = pairs.flatMap(pair => pair);
debug(items);
const dividers = [[[6]], [[2]]];
const sortedItems = items.concat(dividers).sort(compare).reverse();
const indices = [
    sortedItems.indexOf(dividers[0]) + 1,
    sortedItems.indexOf(dividers[1]) + 1,
];
debug(indices);

const decoderKey = indices.reduce((acc, it) => acc * it, 1);
println(`Part 2: ${decoderKey}`);

