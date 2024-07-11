#include <cctype>
#include <functional>
#include <string>
#include <iostream>
#include <ostream>

template<typename Fn>
struct Defer {
  Fn f;
  Defer(Fn f) : f(f) {}
  ~Defer() { f(); }
};

template<typename Fn>
Defer<Fn> defer_func(Fn f) {
  return Defer<Fn>(f);
}

#define DEFER_1(x, y) x##y
#define DEFER_2(x, y) DEFER_1(x, y)
#define DEFER_3(x)    DEFER_2(x, __COUNTER__)
#define defer(code)   auto DEFER_3(_defer_) = defer_func([&](){code;})

using std::cout;
using std::endl;
using std::string;
using std::function;

struct Char {
  char ch;

  Char(char c): ch(c) {}
  const string as_str() const {
    return string(1, ch);
  }

  string operator+(const Char& other) {
    return as_str() + other.as_str();
  }
};

inline string proto_str(const Char ch) {
  return ch.as_str();
}

string apply2(string start, function<string(char)> f) {
  string res = "";
  for (auto ch : start) {
    res += f(ch);
  }

  return res;
}

string capitalize(char c) {
  return Char(toupper(c)).as_str();
}

int main() {
  const function<string(char)> double_char = [&](char c) -> string {
    return Char(c) + Char(c);
  };

  string a = "I am a string";
  string b = apply2(a, capitalize);
  string c =  apply2(a, double_char);
  cout << "a is " << a << endl;
  cout << "b is " << b << endl;
  cout << "c is " << c << endl;
  return 0;
}
