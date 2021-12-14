#!/usr/local/bin/ts-node

const fs = require("fs");

const debug = false;

debug && console.time('the whole thing');
const lines = fs.readFileSync(0).toString().split('\n').filter((l: string) => !!l);

const arg: number = parseInt((process.argv.slice(2))[0] || '2');
const part: number = arg > 1 ? 2 : 1;
const partTwo: boolean = part === 2;
console.log(`Part ${part}: visiting a single small cave twice is${partTwo ? "" : " not"} allowed.`);

const map: { [ key: string]: Array<string>; } = {};
lines
    .map((line: string) => line.split('-'))
    .forEach(([u, v]: [string, string]) => {
        map[u] = map[u] || [];
        map[v] = map[v] || [];
        map[u].push(v);
        map[v].push(u);
    });

class Cave {
    name: string;
    bit: number;
    isSmall: boolean;
    isTerminal: boolean;

    private static ids: Array<string> = [];

    constructor(name: string) {
        this.name = name;
        this.isTerminal = name === 'start' || name === 'end';
        this.isSmall = name.toLowerCase() === name;

        // every cave gets its own id...
        let id = Cave.ids.indexOf(name) + 1; // big and 'end' caves will have id = 0: they never count as seen / visited
        if (this.isSmall && !id && name !== 'end') id = Cave.ids.push(name); // push returns new array length
        debug && console.log(`Assigned id ${id} to cave ${name}`);

        // ...and is represented as one bit in ${id} register
        this.bit = (1 << id) >> 1;
    }
}

class Path {
    head: Cave;
    parent: Path | null;
    everSeenSmallCaveTwice: boolean;
    seen: number; // bitset: one bit per cave

    constructor(head: Cave, parent: Path | null = null) {
        this.head = head;
        this.parent = parent;
        this.everSeenSmallCaveTwice = !!parent && (parent.everSeenSmallCaveTwice || parent?.hasSeen(head));
        this.seen = (parent?.seen || 0) | head.bit; // adding to the set
        debug && console.log(`Path head ${this.head.name} seen ${this.seen} twice ${this.everSeenSmallCaveTwice}`);
    }

    createChild(head: Cave): Path { return new Path(head, this); }

    hasSeen(cave: Cave): boolean { return !!(this.seen & cave.bit); }

    toString(): string { return Array.from(this.traceBack()).reverse().join(","); }

    *traceBack(): Generator<string> {
        let node: Path | null = this;
        while (node) {
            yield node.head.name;
            node = node.parent;
        }
    }
}

const caves: { [key: string]: Cave; } = {}; // index of all the caves by name
Object.entries(map)
    .forEach(([u, vs]: [string, Array<string>]) => {
        caves[u] = caves[u] || new Cave(u);
        vs.forEach((v: string) => { caves[v] = caves[v] || new Cave(v); });
    });

debug && console.log(JSON.stringify(map, null, 2));

function* walk(path: Path): Generator<Path> {
    if (path.head.name === 'end') {
        yield path;
        return;
    }
    debug && console.log(`at ${path.head.name}, next up: ${map[path.head.name] || []}`);
    for (const nextName of map[path.head.name] || []) {
        const next: Cave = caves[nextName]!;
        const canVisitTwice = partTwo && !path.everSeenSmallCaveTwice && next.isSmall && !next.isTerminal;
        if (!path.hasSeen(next) || canVisitTwice) yield* walk(path.createChild(next));
    }
}

const paths: Array<Path> = Array.from(walk(new Path(caves['start'])));
debug && paths.forEach(p => console.log(p.toString()));

console.log(`There are ${paths.length} paths through the tunnels.`);
debug && console.timeEnd('the whole thing');

