#define MAKE_CHAR
#include "t.h"

using std::cout;
// using std::endl;

struct A {
  A() {}
  const void do_stuff(str a) const {
    cout << "A does stuff with " << a << "..\n";
  }
};

struct B {
  int b;

  B(int _b) : b(_b) {}
  void do_stuff(str a) {
    cout << "B(" << b << ") does stuff with " << a << "..\n";
  }
};

template<typename Type>
struct TraitX {
private:
  Type* inst;

public:
  explicit TraitX(Type* i) : inst(i) {}

  void do_stuff(str a) {
    inst->do_stuff(a);
  }
};

template<typename Type>
void do_something(TraitX<Type> x) {
  x.do_stuff("some string");
}

int main() {
  A a;
  B b(420);
  do_something(TraitX<A>(&a));
  do_something(TraitX<B>(&b));

  return 0;
}
