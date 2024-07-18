#define MAKE_CHAR
#include "t.h"

using std::cout;
using std::endl;

int main() {
  f32 a = 1.234;
  f64 b = 2.345;

  cout << proto_str(a) << endl;
  return 0;
}
