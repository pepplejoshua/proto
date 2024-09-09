#include <iostream>
#include <string>
using namespace std;
typedef string str;

struct y {
  int m = 1;
  int n;

  static int int_add(int a, int b) {
    return a + b;
  }
};

const auto add = y::int_add;

int main() {
  auto X = add(2, 2);
  auto Z = y {
    .m = 1,
    .n = 2,
  };
  cout << X << endl;
  return 0;
}
