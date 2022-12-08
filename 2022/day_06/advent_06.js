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

const input = stdinBuffer.toString().slice(0, -1);

String.prototype.isValidMarker = function() {
    const dict = {};
    for (c of this) {
        if (dict[c]) return false;
        dict[c] = true;
    }
    return true;
};

function findMarker(markerLength) {
    for (i = 0; i < input.length - markerLength; i++) {
        const packet = input.slice(i, i + markerLength);
        if (packet.isValidMarker()) return i + markerLength;
    }
}

println(`Part 1: ${findMarker(4)}`);
println(`Part 2: ${findMarker(14)}`);

