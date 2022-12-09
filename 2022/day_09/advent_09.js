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

debug(() => `Read ${lines.length} lines`);

const visited = {};

function visit(coord) {
    visited[JSON.stringify(coord)] = true;
}

let [head, tail] = [[0, 0], [0, 0]];

visit(tail);

for (line of lines) {
    debug(`==== ${line}`);
    const [direction, distance] = line.split(" ");
    const delta = {R: [0, 1], L: [0, -1], U: [-1, 0], D: [1, 0]}[direction];
    for (let i = 0; i < parseInt(distance); i++) {
        head = [ head[0] + delta[0], head[1] + delta[1] ];
        const diff = [ head[0] - tail[0], head[1] - tail[1] ];
        debug(() => `diff ${diff}`);
        const shouldMoveTail = Math.abs(diff[0]) > 1 || Math.abs(diff[1]) > 1;
        if (!shouldMoveTail) continue;
        const move = [ Math.sign(diff[0]), Math.sign(diff[1]) ];
        debug(() => `moving ${move}`);
        tail = [ tail[0] + move[0], tail[1] + move[1] ];
        visit(tail);
    }
}

println(`Part 1: The tail visited ${Object.keys(visited).length} points`);

for (const key in visited) delete visited[key];

let rope = Array.from(Array(10), () => [0, 0]);
debug(rope);

visit(rope.at(-1));
debug(visited);

for (line of lines) {
    const [direction, distance] = line.split(" ");
    const delta = {R: [0, 1], L: [0, -1], U: [-1, 0], D: [1, 0]}[direction];
    for (let i = 0; i < parseInt(distance); i++) {
        rope[0] = [ rope[0][0] + delta[0], rope[0][1] + delta[1] ];
        for (let k = 1; k < rope.length; k++) {
            const leader = rope[k - 1];
            const follower = rope[k];
            const diff = [ leader[0] - follower[0], leader[1] - follower[1] ];
            const shouldMovefollower = Math.abs(diff[0]) > 1 || Math.abs(diff[1]) > 1;
            if (!shouldMovefollower) break;
            const move = [ Math.sign(diff[0]), Math.sign(diff[1]) ];
            rope[k] = [ follower[0] + move[0], follower[1] + move[1] ];
        }
        visit(rope.at(-1));
    }
}

println(`Part 2: The tail visited ${Object.keys(visited).length} points`);

