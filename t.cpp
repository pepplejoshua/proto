#include "t.h"

using std::cout;
using std::endl;

Slice<int> t() {
  auto arr = Array<int, 5>({1, 2, 3, 4, 5});
  return arr.make_slice_from(1);
}

int main() {
  // auto x = Str<12>("hello world!");
  // Slice<char> x_slice = x.make_slice(6, 11);
  // cout << proto_str(x) << endl;
  // cout << proto_str(x_slice) << endl;

  // auto a = Array<int, 5>({0, 0, 0, 0, 0});
  // auto b = Slice<int>();


  // cout << "a was: " << proto_str(a) << endl;
  // cout << "b was: " << proto_str(b) << endl;

  // for (auto i = 0; i < 4; i++) {
  //   cout << "appending " << i + 1 << " to b..." << endl;
  //   b.append(i + 1);
  //   cout << "a is:  " << proto_str(a) << endl;
  //   cout << "b is:  " << proto_str(b) << endl;
  // }


  auto c = t();
  auto d = c;
  d = c;
  // cout << "c is:  " << proto_str(c) << endl;

  return 0;
}
