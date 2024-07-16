#include "t.h"
#include <map>

using std::cout;
using std::endl;
using std::map;

int main() {
  map<str, int> m = {{"hello", 2}};

  for (auto p : m) {
    cout << p.first << " " << p.second << endl;
  }

  return 0;
}
