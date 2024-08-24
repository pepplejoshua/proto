#include <string>

using std::string;

typedef std::string str;

struct Y {
  str a;
};

struct XYZ {
  using Self = XYZ;
  int a;
  int b;
  Y c = {
    .a = "23234"
  };

  // non-member functions are static
  static Self init(int _a, int _b) {
    return Self {
      .a = _a,
      .b = _b,
    };
  }
};

using A = XYZ;

int main() {
  A tuple = A::init(2, 3);
  return 0;
}
