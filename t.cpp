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

  auto x = Str<12>("hello world!");
  Slice<char> x_slice = x.make_slice(6, 11);
  cout << proto_str(x) << endl;
  cout << proto_str(x_slice) << endl;

  auto t = new int(1);

  cout << *t << endl;
  delete t;

  return 0;
}
