#!/usr/local/bin/ts-node

const fs = require("fs");
const lines = fs.readFileSync(0).toString().split('\n').filter((l: string) => !!l);;

const arg: number = parseInt((process.argv.slice(2))[0] || '2');
const part: number = arg > 1 ? 2 : 1;
const partTwo: boolean = part === 2;

const debug = false;

const map: { [ key: string]: Array<string>; } = {};
lines
    .map((line: string) => line.split('-'))
    .forEach((edge: Array<string>) => {
        map[edge[0]] = map[edge[0]] || [];
        map[edge[1]] = map[edge[1]] || [];
        map[edge[0]].push(edge[1]);
        map[edge[1]].push(edge[0]);
    });

interface String {
    isSmall(): boolean;
    isTerminal(): boolean;
}
String.prototype.isSmall = function () { return this.toLowerCase() === this; }
String.prototype.isTerminal  = function () { return this === 'start' || this === 'end'; }

class Path {
    head: string;
    parent: Path | null;
    smallCaveTwiceSeen: string | null;

    constructor(head: string, parent: Path | null = null, smallCaveTwiceSeen: string | null = null) {
        this.head = head;
        this.parent = parent;
        this.smallCaveTwiceSeen = smallCaveTwiceSeen;
    }

    createChild(head: string, smallCaveTwiceSeen: boolean = false): Path {
        return new Path(head, this, smallCaveTwiceSeen ? head : this.smallCaveTwiceSeen);
    }

    seen(v: string): boolean {
        if (!v.isSmall()) return false;
        for (const u of this.backTrack()) if (u === v) return true;
        return false;
    }

    toString(): string { return Array.from(this.backTrack()).reverse().join(","); }

    *backTrack(): Generator<string> {
        let node: Path | null = this;
        while (node) {
            yield node.head;
            node = node.parent;
        }
    }
}

debug && console.log(JSON.stringify(map, null, 2));

function* walk(path: Path): Generator<Path, void, any> {
    if (path.head === 'end') {
        yield path;
        return;
    }
    debug && console.log(`at ${path.head}, next up: ${map[path.head] || []}`);
    for (const next of map[path.head] || []) {
        const canVisitTwice = partTwo && !path.smallCaveTwiceSeen && next.isSmall() && !next.isTerminal();
        if (path.seen(next) && !canVisitTwice) continue;
        yield* walk(path.createChild(next, path.seen(next)));
    }
}

const paths: Array<Path> = Array.from(walk(new Path('start')));

debug && paths.forEach(p => console.log(p.toString()));

console.log(`Part ${part}: visiting a single small cave twice is${partTwo ? "" : " not"} allowed.`);
console.log(`There are ${paths.length} paths through the tunnels.`);

