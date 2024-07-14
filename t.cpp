#include <functional>
#include "t.h"

using std::cout;
using std::endl;
using std::function;

str apply2(str start, function<str(char)> f) {
  str res = "";
  for (auto ch : start) {
    res += f(ch);
  }

  return res;
}

str capitalize(char c) {
  return Char(toupper(c)).as_str();
}

struct A{
  int x;

  A(int x): x{x} {}
};

int main() {
  const function<str(char)> double_char = [&](char c) -> str {
    return Char(c) + Char(c);
  };
  // str a = "I am a str";
  // str b = apply2(a, capitalize);
  // str c =  apply2(a, double_char);
  // cout << "a is " << a << endl;
  // cout << "b is " << b << endl;
  // cout << "c is " << c << endl;

  // auto x = Str<12>("hello world!");
  // Slice<char> x_slice = x.make_slice(6, 11);
  // cout << proto_str(x) << endl;
  // cout << proto_str(x_slice) << endl;

  auto a = new Array<int, 5>({0, 0, 0, 0, 0});
  // auto b = a->make_slice(0, 3);
  auto b = Slice<int>();

  cout << "a was: " << proto_str(*a) << endl;
  cout << "b was: " << proto_str(b) << endl;

  for (auto i = 0; i < 4; i++) {
    cout << "appending " << i + 1 << " to b..." << endl;
    b.append(i + 1);
    cout << "a is:  " << proto_str(*a) << endl;
    cout << "b is:  " << proto_str(b) << endl;
  }

  // auto y = (A*) malloc(sizeof(A) * 10);
  // auto x = Slice<char>();

  delete a;

  return 0;
}
