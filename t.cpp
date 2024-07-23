#define MAKE_CHAR
#include "t.h"

using std::cout;
using std::endl;

typedef PoolAllocator<char> CharAllocator;

Slice<char*> do_things(CharAllocator& pool) {
  auto sl = Slice<char*>(pool.num_of_allocatable_items());
  const int start = 65;
  for (int i = 0; i < pool.num_of_allocatable_items(); i++) {
    auto ch = make_char(start + i);
    Option<char *> dyn_ch = pool.allocate(ch);
    if (dyn_ch.is_none()) {
      cout << i + 1 << ". Cannot allocate char.\n";
    } else {
      sl.append(dyn_ch.unwrap());
    }
  }
  return sl;
}

int main() {
  auto alloc = CharAllocator(13);
  auto sl = do_things(alloc);

  for (auto ch : sl) {
    cout << *ch << endl;
  }
  return 0;
}
