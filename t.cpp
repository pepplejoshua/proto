#include <iostream>

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
    std::cout << x.n << std::endl;

    return 0;
}
