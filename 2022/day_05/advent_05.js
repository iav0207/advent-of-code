#!/usr/local/bin/node

const fs = require("fs");
const stdinBuffer = fs.readFileSync(0); // STDIN

const debugMode = process.argv.includes("-d");
const part2 = process.argv.includes("2");
const part = part2 ? 2 : 1;

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

const image = lines.filter((it) => !it.startsWith("move"));
const stacks = image.pop().match(/\d+/g).map(() => []);
const instructions = lines.filter((it) => it.startsWith("move"));

debug(`${stacks.length} stacks`);

for (line of image.reverse()) {
	let stackNum = 0;
	for (stackNum = 0; stackNum < stacks.length; stackNum++) {
		const item = line[1 + 4 * stackNum];
		if (item.includes(" ")) continue;
		stacks[stackNum].push(item);
	}
}
debug(stacks);


const repeat = (times, fn) => { while(times-- > 0) fn(); }

for (line of instructions) {
	const [count, iFrom, iTo] = line.match(/\d+/g).map((it) => parseInt(it));
	const [from, to] = [stacks[iFrom-1], stacks[iTo-1]];
	if (part < 2) {
		repeat(count, () => to.push(from.pop()));
	} else {
		const buffer = from.splice(from.length - count, count);
		to.push(...buffer);
	}
}

debug(stacks);

println(`Part ${part}: ${stacks.map((s) => s[s.length-1]).join('')}`);

