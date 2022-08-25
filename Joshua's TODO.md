# Joshua's TODO

---

To compile Proto to C++, I need to figure out:

- [ ] equivalent C++ type for all Proto types
  - Tuple can be a vector of \_Type
  - Array is a regular static C++ array
  - Unit is a type that inherits \_Type
  - i64 is int64_t
  - char, bool and string are the builtin C++ types char and string
- [ ] importing external Proto modules
- [ ] compiling a module (main or external) into its corresponding .cpp file
- [ ] handle variadic functions
- [ ] implementing core library directly in Proto, to be compiled to C++
- [ ] when to convert a builtin converted type (e.g: i64 to int64_t) to a \_Type object of equivalent value
- [ ] what the minimum amount of C++ code required is to start of compilation, taking into account the last point

## Examples of compiling Proto to C++:

### loop-based fibonacci solution:

Proto:

```rs
fn fib_loop(n: i64) -> i64 {
    if n <= 1 {
        n
    } else {
        mut x = 0;
        mut y = 1;
        mut z = 0;
        for i in  1..n {
            z = x + y;
            x = y;
            y = z;
        }
        z
    }
}

fn main() -> i64 {
    fib_loop(32)
}
```

Generated C++:

```cpp
int64_t fib_loop(int64_t n) {
  if (n <= 1) {
    return n;
  } else {
    int64_t x = 0;
    int64_t y = 1;
    int64_t z = 0;
    for (int64_t i = 1; i < n; i += 1) {
      z = x + y;
      x = y;
      y = z;
    }
    return z;
  }
}

int64_t _main() {
  return fib_loop(32);
}

int main() {
  cout << _main() << endl;
  return 0;
}
```
