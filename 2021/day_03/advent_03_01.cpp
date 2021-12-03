#include <stdio.h>
#include <bitset>
#include <iostream>
#include <vector>
using namespace std;

string binary(int n)
{
  return bitset<32>(n).to_string();
}

int main(int argc, char **argv)
{
  char x = 0;
  int ones = 0, zeros = 0;
  vector<char> v;
  while ((x = getc(stdin)) != EOF) {
    if (x == '\n') {
      char b = (ones > zeros) ? '1' : '0';
      v.push_back(b);
      printf("%i : %i = %c\n", ones, zeros, b);
      ones = 0;
      zeros = 0;
    }
    else if (x == '1') ones++;
    else zeros++;
  }
  string s(v.begin(), v.end());
  int mask = (1 << v.size()) - 1;
  int gamma = stoi(s, nullptr, 2);
  int epsilon = gamma ^ mask;
  printf("mask    %s\n", binary(mask).c_str());
  printf("gamma   %s = %d\n", binary(gamma).c_str(), gamma);
  printf("epsilon %s = %d\n", binary(epsilon).c_str(), epsilon);
  printf("%d * %d = %d", gamma, epsilon, gamma * epsilon);
  return 0;
}

