#include <algorithm>
#include <cctype>
#include <cstdarg>
#include <iostream>
#include <map>
#include <string>
#include <vector>
using namespace std;

class Solution {
public:
	Solution(bool debugMode) : debugMode(debugMode) {}

	void solve(vector<string> input) {
		printf("Part 1: %d\n", part1(input));
		printf("Part 2: %d\n", part2(input));
	}

private:
	bool debugMode;

	int part1(vector<string> input) {
		return -1;
	}

	int part2(vector<string> input) {
		return -1;
	}

	void debug(const char* format, ...) {
		if (!debugMode) return;
		va_list args;
		va_start(args, format);
		vprintf(format, args);
		va_end(args);
	}
};

vector<string> readInputs() {
	vector<string> input;
	for (string line; getline(cin, line);) {
		input.push_back(line);
	}
	return input;
}

int main(int argc, char **argv) {
	bool debugMode = find(argv + 1, argv + argc, string("-d")) != argv + argc;
	Solution(debugMode).solve(readInputs());
	return 0;
}

