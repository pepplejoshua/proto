#define MAKE_CHAR
#include "t.h"

using std::cout;
using std::endl;

void do_things(BumpAllocator& bump_alo) {
  Slice<int*> alloc(5);
  for (int i = 0; i < alloc.cap(); i++) {
    Option<int*> a = bump_alo.allocate<int>(300);
    if (a.is_none()) {
      cout << i + 1 << ". Error allocating\n";
    } else {
      alloc.append(a.unwrap());
    }
  }

  Option<Array<char, 5> *> arr = bump_alo.allocate<Array<char, 5>>({'a', 'b', 'c', 'd', 'e'});
  auto array = arr.unwrap();
  cout << proto_str(*array) << endl;
}

int main() {
  // allocate 1KB
  auto bump_alo = BumpAllocator(1024);
  do_things(bump_alo);
  cout << "HERE" << endl;
  return 0;
}
