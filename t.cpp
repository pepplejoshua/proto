#include <string>

using std::string;

typedef std::string str;

struct Y {
  str a;
};

struct XYZ {
  int a;
  int b;
  Y c = {
    .a = "23234"
  };

  // non-member functions are static
  static XYZ init(int _a, int _b) {
    return XYZ {
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
