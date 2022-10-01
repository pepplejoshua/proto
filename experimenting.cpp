#include <iostream>
#include "D:\Programming\proto\prelude\prelude.hpp"
using namespace std;
Proto_Unit __main() {
    return Proto_Unit();
}


#ifdef PROTO_TESTING
void test_HelloWorld() {
    const int64_t a = 1 + 2;
}
#endif

int main() {
    cout << __main() << endl;
    return 0;
}

