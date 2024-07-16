#include <cstring>
#include <initializer_list>
#include <iostream>
#include <string>
#include <cstdlib>
#include <cstdint>
#include <map>

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
    uint_pr length;
    uint_pr arr_capacity;
    bool allocates;

public:
    Slice(T* s, uint_pr len, uint_pr cap)
      : start(s), length(len), arr_capacity(cap), allocates(false) {
      // std::cout << "Slice(T* s, uint_pr len, uint_pr cap) constructor called\n";
    }

    Slice(uint_pr cap)
      : length(0), arr_capacity(cap), allocates(true) {
      start = (T*) malloc(sizeof(T) * cap);
      std::memset(start, 0, cap);
      // std::cout << "Slice(uint_pr cap) constructor called\n";
      // std::cout << "allocated memory for " << cap << " items...\n";
    }

    Slice() : Slice(16) {
      // std::cout << "Slice() constructor called\n";
    }

    // Copy constructor
    Slice(const Slice& other)
      : start(other.start),  length(other.length),
        arr_capacity(other.arr_capacity), allocates(false) {
      // std::cout << "Slice(const Slice& other) constructor called\n";
    }

    Slice& operator=(const Slice& other) {
      if (this == &other) { return *this; }
      // std::cout << "operator=(const Slice& other) constructor called\n";
      start = other.start;
      length = other.length;
      arr_capacity = other.arr_capacity;
      allocates = false;
      return *this;
    }

    ~Slice() {
      if (allocates) {
        free(start);
        // std::cout << "cleaning up Slice" << std::endl;
      }
    }

    constexpr uint_pr len() const noexcept {
        return length;
    }

    constexpr uint_pr cap() const noexcept {
        return arr_capacity;
    }

    void append(T item) {
      if (length >= arr_capacity) {
        // Double the current capacity and align to the next power of two
        uint_pr new_capacity = next_power_of_two(arr_capacity * 2);
        T* new_array = (T*) malloc(sizeof(T) * new_capacity);
        std::memcpy(new_array, start, arr_capacity * sizeof(T));

        if (allocates) {
            free(start);
        }

        start = new_array;
        arr_capacity = new_capacity;
        allocates = true;

        // std::cout << "allocated memory for " << arr_capacity << " items...\n";
      }

      start[length] = item;
      length += 1;
    }

    // Helper function to find the next power of two
    static uint_pr next_power_of_two(uint_pr n) {
        if (n == 0) return 1;
        n--;
        n |= n >> 1;
        n |= n >> 2;
        n |= n >> 4;
        n |= n >> 8;
        n |= n >> 16;
        n |= n >> 32;
        return n + 1;
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
        return Slice<T>(this->start + start, end_exclusive - start, arr_capacity - start);
    }

    inline const Slice<T> make_slice(uint_pr start, uint_pr end_exclusive) const {
        if (start >= length || end_exclusive > length || start >= end_exclusive) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(this->start + start, end_exclusive - start, arr_capacity - start);
    }

    inline Slice<T> make_slice_from(uint_pr start) {
        if (start >= length) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(this->start + start, length - start, arr_capacity - start);
    }

    inline const Slice<T> make_slice_from(uint_pr start) const {
        if (start >= length) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(this->start + start, length - start, arr_capacity - start);
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
        return Slice<T>(data + start, end_exclusive - start, len() - start);
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
        return Slice<T>((T*)data + start, end_exclusive - start, len() - start);
    }

    inline Slice<T> make_slice_from(uint_pr start) {
        if (start >= N) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(data + start, len() - start, len() - start);
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
        return Slice<T>((T*)data, len() - start, len() - start);
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

template<typename Key, typename Value>
class HashMap {
// private:

public:
  std::map<Key, Value> contents;
  HashMap(std::initializer_list<typename std::pair<const Key, Value>> init) : contents(init) {}

  inline constexpr uint_pr len() const noexcept {
      return contents.size();
  }

  Value& operator[](Key key) {
    return contents[key];
  }

  const Value& operator[](Key key) const {
    return contents[key];
  }

  Option<Value> get(Key key) {
    const auto exists = contents.find(key);
    if (exists == contents.end()) {
      return Option<Value>();
    }

    return Option<Value>(exists->second);
  }

  const Option<Value> get(Key key) const {
    const auto exists = contents.find(key);
    if (exists == contents.end()) {
      return Option<Value>();
    }

    return Option<Value>(exists->second);
  }

  bool contains(Key key) const {
    return get(key).is_some();
  }

  str as_str() {
    str s = "{ ";
    auto i = 0;
    for (const auto& [key, val] : *this) {
      defer(i += 1);
      s += "(";
      s += proto_str(key);
      s += ", ";
      s += proto_str(val);
      s += ")";

      if (i + 1 < len()) {
        s += ", ";
      }
    }
    s += " }";
    return s;
  }

  // Begin and end methods for range-based for loops
  inline auto begin() noexcept { return contents.begin(); }
  inline auto end() noexcept { return contents.end(); }
  inline const auto begin() const noexcept { return contents.begin(); }
  inline const auto end() const noexcept { return contents.end(); }
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
  return "*Ptr<" + std::to_string(reinterpret_cast<uintptr_t>(t)) + ">";
}

#include <type_traits>
#include <utility>
// Helper template to check if a type has an as_str() method
template <typename T>
class has_as_str {
private:
    template <typename U>
    static auto test(int) -> decltype(std::declval<U>().as_str(), std::true_type());

    template <typename>
    static std::false_type test(...);

public:
    static constexpr bool value = decltype(test<T>(0))::value;
};

// Overload for types with an as_str() method
template <typename T>
inline auto proto_str(T& t) -> typename std::enable_if<has_as_str<T>::value, str>::type {
    return t.as_str();
}

// Fallback for other types (e.g., for demonstration)
template <typename T>
inline auto proto_str(T& t) -> typename std::enable_if<!has_as_str<T>::value, str>::type {
    return "<unprintable>";
}
