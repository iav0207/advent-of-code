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

const commands = stdinBuffer.toString()
    .split("\n$ ")
    .map((it) => it.split("\n"))
    .filter((it) => !!it);

debug(() => `Read ${commands.length} commands`);

const createNode = (patch = {}) => ({
    name: undefined,
    parent: undefined,
    fileSizes: [],
    children: [],
    ...patch,
});

const root = createNode({name: '', parent: null});

let cwd = root;

function handleCd(arg) {
    if (arg === '/') {
        return root;
    }
    if (arg === '..') {
        const newCwd = cwd.parent || createNode();
        if (!newCwd.children.some((c) => c.name === cwd.name)) newCwd.children.push(cwd);
        cwd.parent = newCwd;
        return newCwd;
    }
    const known = cwd.children.find((it) => it.name === arg);
    if (arg === 'e') debug(`known child: ${known}`);
    const newCwd = known || createNode({
        name: arg,
        parent: cwd,
    });
    debug(`this.children: ${cwd.children.map((it) => it.name)}`);
    debug(`newCwd: ${newCwd.name}`);
    debug(`newCwd.children: ${newCwd.children.map((it) => it.name)}`);
    if (!known) cwd.children.push(newCwd);
    return newCwd;
}

function handleLs(output) {
    const dirNames = output
        .filter((it) => it.startsWith("dir "))
        .map((it) => it.slice(4));

    const createDirCount = dirNames.length - cwd.children.length;
    for (i = 0; i < createDirCount; i++) cwd.children.push(createNode({parent: cwd}));

    const anonymousChildren = cwd.children.filter((it) => it.name === undefined);
    const unassignedNames = dirNames.filter((name) =>
        cwd.children.some((child) => child.name === name)
    );
    if (unassignedNames.length === 1 && anonymousChildren.length === 1) {
        anonymousChildren[0].name === unassignedNames[0];
    }
    cwd.fileSizes = output
        .filter((it) => /^\d+/.test(it))
        .map((it) => parseInt(it.split(" ")[0]));
}

for (command of commands) {
    if (command[0].startsWith("cd ")) {
        debug(`$ ${command[0]}`);
        cwd = handleCd(command[0].split(" ")[1]);
        debug(cwd.name);
    } else handleLs(command.slice(1));
}

Array.prototype.sum = function() { return this.reduce((acc, it) => acc + it, 0); };

let smallDirsSize = 0;

function countTotalSizeOf(directory) {
    ret =
        directory.fileSizes.sum()
            + directory.children.map((child) => totalSizeOf(child)).sum();
    if (ret <= 100000) smallDirsSize += ret;
    debug(`Size of ${directory.name} is ${ret}`);
    if (directory.name === 'a') {
        debug(`fileSizes: ${directory.fileSizes} = ${directory.fileSizes.sum()}`);
        debug(directory.children.map((it) => it.name));
    }
    return ret;
}

function totalSizeOf(directory) {
    directory.totalSize = directory.totalSize || countTotalSizeOf(directory);
    return directory.totalSize;
}

totalSizeOf(root);

println(`Part 1: ${smallDirsSize}`);

