#include <iostream>
#include <string>
using namespace std;
typedef string str;

struct Y {
  str a;
};

struct XYZ {
  using Self = XYZ;
  int a;
  int b;
  const Y c = {
    .a = "2334",
  };

  // non-member functions are static
  static Self init(int _a, int _b) {
    return Self {
      .a = _a,
      .b = _b,
    };
  }

  void print() {
    cout << "XYZ { a: " << this->a << ", b: " << this->b << "}\n";
  }
};

using A = XYZ;

int main() {
  A tuple = A::init(2, 3);
  tuple.print();
  return 0;
}
