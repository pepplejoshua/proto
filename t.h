#include <iostream>
#include <string>
#include <cstdlib>
#include <cstdint>

typedef uint64_t uint_pr;
typedef std::string str;

template <typename T>
inline typename std::enable_if<std::is_arithmetic<T>::value, str>::type
proto_str(T value) {
    return std::to_string(value);
}

inline str proto_str(const str s) {
    return "\"" + s + "\"";
}

inline str proto_str(const char c) {
    return str(1, c);
}

inline str proto_str(const bool b) {
    return b ? "true" : "false";
}

inline void panic(int line, const str sourcefile, const str msg) {
    // should unwind the call stack and then show the error message
    std::cout << sourcefile << ":" << line << ":" << " " << msg << std::endl;
    std::exit(EXIT_FAILURE);
}

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

template<typename T>
class Option {
private:
    T data;

public:
    enum {
        Some,
        None
    } tag;

    Option(T item) : data(item) {
        tag = Some;
    }

    Option() : data() {
        tag = None;
    }

    const bool is_some() const {
        return tag == Some;
    }

    bool is_some() {
        return tag == Some;
    }

    const bool is_none() const {
        return tag == None;
    }

    bool is_none() {
        return tag == None;
    }

    inline T& unwrap() {
        // should use a panic() function to fail if Option.is_none() is true
        if (this->is_none()) {
            panic(__LINE__, __FILE_NAME__, "attempted to unwrap an Option::None.");
        }
        return data;
    }

    inline const T& unwrap() const {
        // should use a panic() function to fail if Option.is_none() is true
        if (this->is_none()) {
            panic(__LINE__, __FILE_NAME__, "attempted to unwrap an Option::None.");
        }
        return data;
    }
};

template<typename T>
inline str proto_str(const Option<T> op) {
    str s = "";
    if (op.is_some()) {
        s += "some ";
        s += proto_str(op.unwrap());
    } else {
        s = "none";
    }
    return s;
}
template<typename T>
class Slice {
private:
    T* start;
    bool allocates;
    uint_pr length;
    uint_pr arr_capacity;

public:
    Slice(T* s, uint_pr len, uint_pr cap)
      : start(s), length(len), arr_capacity(cap), allocates(false) {}

    Slice(T* s, uint_pr len, uint_pr cap, bool allocs)
      : start(s), length(len), arr_capacity(cap), allocates(allocs) {
    }

    ~Slice() {
      if (allocates) {
        delete start;
        std::cout << "cleaning up Slice" << std::endl;
      }
    }

    constexpr uint_pr len() const noexcept {
        return length;
    }

    T& operator[](uint_pr index) {
        return start[index];
    }

    const T& operator[](uint_pr index) const {
        return start[index];
    }

    Option<T> get(uint_pr index) {
        if (index >= length) {
            return Option<T>();
        }

        return Option<T>(start[index]);
    }

    const Option<T> get(uint_pr index) const {
        if (index >= length) {
            return Option<T>();
        }

        return Option<T>(start[index]);
    }

    inline Slice<T> make_slice(uint_pr start, uint_pr end_exclusive) {
        if (start >= length || end_exclusive > length || start >= end_exclusive) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(this->start + start, end_exclusive - start, arr_capacity);
    }

    inline const Slice<T> make_slice(uint_pr start, uint_pr end_exclusive) const {
        if (start >= length || end_exclusive > length || start >= end_exclusive) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(this->start + start, end_exclusive - start, arr_capacity);
    }

    inline Slice<T> make_slice_from(uint_pr start) {
        if (start >= length) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(this->start + start, length - start, arr_capacity);
    }

    inline const Slice<T> make_slice_from(uint_pr start) const {
        if (start >= length) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(this->start + start, length - start, arr_capacity);
    }

    // Begin and end methods for range-based for loops
    inline T* begin() noexcept { return start; }
    inline T* end() noexcept { return start + length; }
    inline const T* begin() const noexcept { return start; }
    inline const T* end() const noexcept { return start + length; }
};

template<typename T, uint_pr N>
class Array {
private:
    T data[N] = {};

public:
    Array(std::initializer_list<T> init) {
        std::copy(init.begin(), init.end(), data);
    }
    Array(const char* str) {
        std::size_t length = std::min(std::strlen(str), static_cast<size_t>(N));
        std::copy(str, str + length, data);
    }
    Array() {}

    T& operator[](std::size_t index) {
        return data[index];
    }

    const T& operator[](uint_pr index) const {
        return data[index];
    }

    Option<T> get(uint_pr index) {
        if (index >= N) {
            return Option<T>();
        }

        return Option<T>(data[index]);
    }

    const Option<T> get(uint_pr index) const {
        if (index >= N) {
            return Option<T>();
        }

        return Option<T>(data[index]);
    }

    inline Slice<T> make_slice(uint_pr start, uint_pr end_exclusive) {
        if (start >= N || end_exclusive > N || start >= end_exclusive) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(data + start, end_exclusive - start, len());
    }

    inline const Slice<T> make_slice(uint_pr start, uint_pr end_exclusive) const {
        if (start >= N || end_exclusive > N || start >= end_exclusive) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        // HACK:
        // when you make a slice of a constant array, we make a copy of the array;
        // std::copy(data, data + len(), start_copy);
        // OR UNSAFE HACK:
        // grab data and unsafe cast it to a T*. this is Sparta!
        T* start_copy = (T*)data;
        return Slice<T>(start_copy + start, end_exclusive - start, len());
    }

    inline Slice<T> make_slice_from(uint_pr start) {
        if (start >= N) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(data + start, len() - start, len());
    }

    inline const Slice<T> make_slice_from(uint_pr start) const {
        if (start >= N) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        // HACK:
        // when you make a slice of a constant array, we make a copy of the array;
        // std::copy(data, data + len(), start_copy);
        // OR UNSAFE HACK:
        // grab data and unsafe cast it to a T*. this is Sparta!
        T* start_copy = (T*)data;
        return Slice<T>(start_copy, len() - start, len());
    }

    inline constexpr uint_pr len() const noexcept {
        return N;
    }

    // Begin and end methods for range-based for loops
    inline T* begin() noexcept { return data; }
    inline T* end() noexcept { return data + N; }
    inline const T* begin() const noexcept { return data; }
    inline const T* end() const noexcept { return data + N; }
};

template<typename T>
inline str proto_str(const Slice<T> slice) {
    int count = 0;
    str s = "[";
    for (T a : slice) {
        s += proto_str(a);
        count++;
        if (!(count == slice.len())) {
            s += ", ";
        }
    }
    s += "]";
    return s;
}

template<typename T, uint_pr N>
inline str proto_str(const Array<T, N> arr) {
    int count = 0;
    str s = "[";
    for (T a : arr) {
        s += proto_str(a);
        count++;
        if (!(count == N)) {
            s += ", ";
        }
    }
    s += "]";
    return s;
}
inline void proto_println(const str s) {
    std::cout << s << std::endl;
}

inline void proto_print(const str s) {
    std::cout << s;
}

struct Char {
  char ch;

  Char(char c): ch(c) {}
  const str as_str() const {
    return str(1, ch);
  }

  str operator+(const Char& other) {
    return as_str() + other.as_str();
  }
};

template<uint_pr N>
using Str = Array<char, N>;

inline str proto_str(const Char ch) {
  return ch.as_str();
}

template<typename T>
inline str proto_str(const T* t) {
  return "*<" + std::to_string(reinterpret_cast<uintptr_t>(t)) + ">";
}
