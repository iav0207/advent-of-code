#!/usr/local/bin/node

const fs = require("fs");
const stdinBuffer = fs.readFileSync(0); // STDIN

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

const lines = stdinBuffer.toString().split("\n").filter((it) => !!it);

debug(`Read ${lines.length} pairs`);
debug(() => lines);

Array.prototype.fullyContains = function(other) {
    return this[0] <= other[0] && this[1] >= other[1];
};

Array.prototype.overlaps = function(other) {
    const overlapsLeft = this[0] <= other[0] && this[1] >= other[0];
    const overlapsRight = this[0] <= other[1] && this[1] >= other[1];
    const includesOther = this.fullyContains(other);
    const isIncludedByOther = other.fullyContains(this);
    return overlapsLeft || overlapsRight || includesOther || isIncludedByOther;
};

String.prototype.asIntervals = function() {
    return this.split(",").map((part) => part.split("-").map((it) => parseInt(it)));
};

String.prototype.hasFullInclusions = function() {
    const intervals = this.asIntervals();
    return intervals[0].fullyContains(intervals[1]) || intervals[1].fullyContains(intervals[0]);
};

String.prototype.hasOverlaps = function() {
    const intervals = this.asIntervals();
    return intervals[0].overlaps(intervals[1]);
};

println(`Part 1: ${lines.filter((it) => it.hasFullInclusions()).length}`);
println(`Part 2: ${lines.filter((it) => it.hasOverlaps()).length}`);

