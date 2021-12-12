#!/usr/local/bin/ts-node

const fs = require("fs");
const lines = fs.readFileSync(0).toString().split('\n').filter((l: string) => !!l);;

const debug = false;

const map: {[ key: string]: Array<string>; } = {};
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

class Node {
    val: string;
    parent: Node | null;
    visited: Set<string>;
    visitedTwice: string | null;
    constructor(val: string, parent: Node | null = null, visited: Set<string> = new Set(), visitedTwice: string | null = null) {
        this.val = val;
        this.parent = parent;
        this.visited = new Set(visited);
        this.visitedTwice = visitedTwice;
        this.val.isSmall() && this.visited.add(this.val);
    }

    createChild(val: string, visitedTwice: boolean = false): Node {
        return new Node(val, this, this.visited, visitedTwice ? val : this.visitedTwice);
    }

    toString(): string {
        const backTrack: Array<string> = [];
        let node: Node | null = this;
        while (node) {
            backTrack.push(node.val);
            node = node.parent;
        }
        return backTrack.reverse().join(",");
    }
}

const paths: Array<Node> = [];

debug && console.log(JSON.stringify(map, null, 2));

function walk(v: Node) {
    paths.push(v);
    if (v.val === 'end') return;
    debug && console.log(`at ${v.val}, next up: ${map[v.val] || []}`);
    for (const u of map[v.val] || []) {
        const canVisitTwice = !v.visitedTwice && u.isSmall() && !u.isTerminal();
        if (!v.visited.has(u)) walk(v.createChild(u));
        else if (canVisitTwice) walk(v.createChild(u, true));
    }
}

walk(new Node('start'));

const finishingPaths = paths.filter(p => p.val === 'end');

debug && finishingPaths.forEach(p => console.log(p.toString()));
console.log(`There are ${finishingPaths.length} paths through the tunnels.`);

