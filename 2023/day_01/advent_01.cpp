#include <iostream>
#include <string>
#include <cctype>
#include <vector>
#include <map>
using namespace std;

class Solution {
public:
	void solve(vector<string> input) {
		printf("Part 1: %d\n", part1(input));
		printf("Part 2: %d\n", part2(input));
	}

private:
	int part1(vector<string> lines) {
		int total = 0;
		for (string& line : lines) {
			vector<int> digits;
			for (char& c : line) {
				if (isdigit(c)) {
					digits.push_back(c - '0');
				}
			}
			total += digits.front() * 10 + digits.back();
		}
		return total;
	}

	int part2(vector<string> lines) {
		int total = 0;
		for (string& line : lines) total += lineValue(line);
		return total;
	}

	int lineValue(string line) {
		map<size_t, int> found;
		for (auto& sub : subs) {
			for (size_t& idx : findSubstringIndexes(line, sub.first)) {
				found[idx] = sub.second;
			}
		}
		return 10 * found.begin()->second + found.rbegin()->second;
	}

	vector<size_t> findSubstringIndexes(const string& s, const string& substr) {
		vector<size_t> indexes;
		size_t pos = s.find(substr);
		while (pos != string::npos) {
			indexes.push_back(pos);
			pos = s.find(substr, pos + 1);
		}
		return indexes;
	}

	map<string, int> subs = {
		{"one", 1},
		{"two", 2},
		{"three", 3},
		{"four", 4},
		{"five", 5},
		{"six", 6},
		{"seven", 7},
		{"eight", 8},
		{"nine", 9},
		{"1", 1},
		{"2", 2},
		{"3", 3},
		{"4", 4},
		{"5", 5},
		{"6", 6},
		{"7", 7},
		{"8", 8},
		{"9", 9},
	};

};

vector<string> readInputs() {
	vector<string> input;
	for (string line; getline(cin, line);) {
		input.push_back(line);
	}
	return input;
}

int main(int argc, char **argv) {
	Solution().solve(readInputs());
	return 0;
}

