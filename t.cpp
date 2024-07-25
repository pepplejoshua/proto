#define MAKE_CHAR
#include "t.h"

using std::cout;
using std::endl;

struct A {
  A() {}
  void do_stuff(str a) {
    cout << "A does stuff with " << a << "..\n";
  }
};

struct B {
  int b;

  B(int _b) : b(_b) {}
  void do_stuff(str a) {
    cout << "B(" << b << ") does stuff with " << a << "..\n";
  }
  void do_thing() {
    cout << "B(" << b << ") is doing thing..\n";
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
struct TraitY {
private:
  Type* inst;

public:
  explicit TraitY(Type* i) : inst(i) {}

  void do_thing() {
    return inst->do_thing();
  }
};

template<typename Type>
struct TraitX_Y : public TraitX<Type>, TraitY<Type> {
public:
  explicit TraitX_Y(Type *i) : TraitX<Type>(i), TraitY<Type>(i) {}

  void do_stuff(str a) {
    return TraitX<Type>::do_stuff(a);
  }

  void do_thing() {
    return TraitY<Type>::do_thing();
  }
};

template<typename Type>
void do_something(TraitX<Type> x) {
  x.do_stuff("some string");
}

template<typename Type>
void do_2_things(TraitX_Y<Type> x) {
  x.do_stuff("some string");
  x.do_thing();
}

int main() {
  A a;
  B b(420);
  do_something(TraitX<A>(&a));
  do_something(TraitX<B>(&b));
  do_2_things(TraitX_Y<B>(&b));
  cout << sizeof(TraitX_Y<B>(&b)) << endl;

  return 0;
}
