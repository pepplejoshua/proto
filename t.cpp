#include <iostream>

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

struct A {
  int n;
  A& self = *this;

  A(int num) {
    n = num;
  }

  void a() {
    self.n = 300;
  }
};

int main() {
    A x = A(3);

    x.a();
    defer(defer(std::cout << "Hello!\n";));
    defer(std::cout << x.n << std::endl);
    std::cout << "printing here" << std::endl;
    defer(std::cout << "printing there" << std::endl);

    return 0;
}
