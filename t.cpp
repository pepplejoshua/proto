#include <iostream>
#include <string>
using namespace std;
typedef string str;

struct y {
  static int int_add(int a, int b) {
    return a + b;
  }
};

const auto add = y::int_add;

int main() {
  X = add(2, 2);
  cout << X << endl;
  return 0;
}
