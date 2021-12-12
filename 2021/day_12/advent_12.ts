#!/usr/local/bin/ts-node

const fs = require("fs");
const lines = fs.readFileSync(0).toString().split('\n').filter((l: string) => !!l);;

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
}
String.prototype.isSmall = function () { return this.toLowerCase() === this; }

class Node {
    val: string;
    parent: Node | null;
    visited: Set<string>;
    constructor(val: string, parent: Node | null, visited: Set<string>) {
        this.val = val;
        this.parent = parent;
        this.visited = new Set(visited);
        this.val.isSmall() && this.visited.add(this.val);
    }

    createChild(val: string): Node { return new Node(val, this, this.visited); }

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

console.log(JSON.stringify(map, null, 2));

function walk(v: Node) {
    paths.push(v);
    if (v.val === 'end') return;
    console.log(`at ${v.val}, next up: ${map[v.val] || []}`);
    for (let u of map[v.val] || []) {
        if (!v.visited.has(u)) walk(v.createChild(u));
    }
}

walk(new Node('start', null, new Set()));

const finishingPaths = paths.filter(p => p.val === 'end');

finishingPaths.forEach(p => console.log(p.toString()));
console.log(`There are ${finishingPaths.length} paths through the tunnels.`);

