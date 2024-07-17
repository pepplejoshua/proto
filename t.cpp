#define MAKE_CHAR
#include "t.h"

using std::cout;
using std::endl;

int main() {
  HashMap<str, int> a = {{"hello", 1}, {"hi", 2}};
  cout << proto_str(a) << endl;
  cout << proto_str(a.contains("non-existent")) << endl;

  cout << make_char(87) << endl;
  return 0;
}
