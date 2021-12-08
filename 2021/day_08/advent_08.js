#!/usr/local/bin/node

const fs = require("fs");
const stdinBuffer = fs.readFileSync(0); // STDIN
const lines = stdinBuffer.toString().split("\n");

const simpleLengths = [2, 4, 3, 7];

let simpleDigitsCount = 0;
let outputsSum = 0;

String.prototype.sortChars = function () { return this.split('').sort().join(''); };
String.prototype.includesAllCharsOf = function (other) { return other.split('').every(c => this.includes(c)); };

const mapByLength = (arr) => arr.reduce((acc, it) => {
    acc[it.length] = acc[it.length] || [];
    acc[it.length].push(it);
    return acc;
}, {});

Array.prototype.remove = function (where) {
    const found = this.find(where);
    this.splice(this.indexOf(found), 1);
    return found;
};

for (const line of lines) {
    if (!line) break;
    const patterns = line.split(" | ")[0].split(" ").map(it => it.sortChars());
    const output = line.split(" | ")[1].split(" ").map(it => it.sortChars());
    simpleDigitsCount += output.filter(o => simpleLengths.includes(o.length)).length;
    const digits = new Array(10);
    // console.log(JSON.stringify(mapByLength(patterns), null, 2));

    digits[1] = patterns.remove(p => p.length === 2);
    digits[4] = patterns.remove(p => p.length === 4);
    digits[7] = patterns.remove(p => p.length === 3);
    digits[8] = patterns.remove(p => p.length === 7);
    digits[3] = patterns.remove(p => p.length === 5 && p.includesAllCharsOf(digits[1]));
    digits[9] = patterns.remove(p => p.length === 6 && p.includesAllCharsOf(digits[4]));
    digits[0] = patterns.remove(p => p.length === 6 && p.includesAllCharsOf(digits[7]));
    digits[6] = patterns.remove(p => p.length === 6);
    digits[5] = patterns.remove(p => digits[6].includesAllCharsOf(p));
    digits[2] = patterns.remove(p => true);

    const outputNum = parseInt(output.map((o, i) => digits.indexOf(o)).join(''));
    // console.log(`digits recognized: ${digits}`);
    // console.log(`unrecognized patterns: ${JSON.stringify(mapByLength(patterns), null, 2)}`);
    // console.log(`output ${output} number: ${outputNum}`);
    
    outputsSum += outputNum;
}
console.log(`Count of simple digits: ${simpleDigitsCount}`);
console.log(`Sum of all outputs is ${outputsSum}`);

