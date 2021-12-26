#!/usr/local/bin/node

const fs = require("fs");
const assert = require("assert");

const input = fs.readFileSync('input.txt', 'utf-8');// fs.readFileSync(0).toString();
const lines = input.split("\n").filter(line => !!line);

function debug(title, smth, space= 0) {
    console.log(`${title}:`.padEnd(15, ' ') + JSON.stringify(smth, null, space));
}

const range = (arr) => [...arr.keys()];

function debug2d(arr) {
    let i = 0;
    console.log(range(arr).map(iscanner => `|Scanner ${iscanner}`.padEnd(6 * dimensions)).join(''));
    // ACHTUNG they can have different number of points
    const maxLen = arr.map(sub => sub.length).reduce((acc, it) => Math.max(acc, it));
    for (let ipoint = 0; ipoint < maxLen; ipoint++) {
        console.log(range(arr).flatMap(iscanner => arr[iscanner][ipoint].map(it => it.toString().padStart(6, ' '))).join(''));
    }
}

const scanners = [];

for (const line of lines) {
    if (line.includes('scanner')) {
        scanners.push([]);
        continue;
    }
    const point = line.split(',').map(it => parseInt(it));
    scanners[scanners.length - 1].push(point);
    scanners[scanners.length - 1].push(point);
}

const dimensions = scanners[0][0].length;

function comparePoints(a, b) {
    for (let i = 0; i < dimensions; i++) if (a[i] !== b[i]) return a[i] - b[i];
    return 0;
}

function pointsEqual(a, b) { return comparePoints(a, b) === 0; }

// sort points within each scanner
debug("Before sort", scanners);
scanners.forEach(it => it.sort(comparePoints));
debug("After sort", scanners);

const deltas = scanners.map(scanner => {
        let d = []/*[scanner[0]]*/;
        let prev = scanner[0];
        for (let i = 0; i < scanner.length; i++) {
            d.push(scanner[i].map((it, j) => it - prev[j]));
            prev = scanner[i]
        }
        return d;
    }
)

Array.prototype.intersect = function (other) { return this.filter(it => other.some(that => pointsEqual(it, that))); }

// This region can be reconstructed by finding pairs of scanners that have overlapping detection regions
// such that there are at least 12 beacons that both scanners detect within the overlap.
// By establishing 12 common beacons, you can precisely determine where the scanners are relative to each other,
// allowing you to reconstruct the beacon map one scanner at a time.

// what if we can take cumulative sums of coordinates along the delta arrays

// or just brute force the permutations :shrug:

// take the first scanner as absolute point of reference (it's in the 'right' orientation)
// until all 'right' transformations of the scanners has been found:
//   for 1 until Nscanners:
//     among all transformations of the scanner try to find one that has an overlap >= 12 beacons
//     this orientation becomes 'right' for this one


function* pointTransformations(point) {
    yield [ point[0],  point[1],  point[2]];
    yield [ point[0],  point[1], -point[2]];
    yield [ point[0], -point[1],  point[2]];
    yield [ point[0], -point[1], -point[2]];
    yield [-point[0],  point[1],  point[2]];
    yield [-point[0],  point[1], -point[2]];
    yield [-point[0], -point[1],  point[2]];
    yield [-point[0], -point[1], -point[2]];

    yield [ point[2],  point[0],  point[1]];
    yield [ point[2],  point[0], -point[1]];
    yield [ point[2], -point[0],  point[1]];
    yield [ point[2], -point[0], -point[1]];
    yield [-point[2],  point[0],  point[1]];
    yield [-point[2],  point[0], -point[1]];
    yield [-point[2], -point[0],  point[1]];
    yield [-point[2], -point[0], -point[1]];

    yield [ point[1],  point[2],  point[0]];
    yield [ point[1],  point[2], -point[0]];
    yield [ point[1], -point[2],  point[0]];
    yield [ point[1], -point[2], -point[0]];
    yield [-point[1],  point[2],  point[0]];
    yield [-point[1],  point[2], -point[0]];
    yield [-point[1], -point[2],  point[0]];
    yield [-point[1], -point[2], -point[0]];
}

function* scannerTransformations(points) {
    const generators = points.map(p => pointTransformations(p));
    for (let t = 0; t < 24; t++) {
        yield generators.map(g => g.next());
    }
}

const intersectionThresh = Math.min(scanners[0].length, 12);

for (let i = 0; i < scanners.length; i++) {
    for (let j = i + 1; j < scanners.length; j++) {
        for (const jDeltaTransformation of scannerTransformations(deltas[j])) {
            const intersection = deltas[i].intersect(jDeltaTransformation).length;
            if (intersection >= intersectionThresh) {
                console.log(`Scanners ${i} and ${j} see ${intersection} beacons that are supposedly the same.`);
                break;
            }
        }
    }
}

debug('Deltas', deltas);

debug2d(deltas);

