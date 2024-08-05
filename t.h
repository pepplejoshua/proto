#include <cstddef>
#include <cstring>
#include <initializer_list>
#include <iostream>
#include <memory>
#include <string>
#include <cstdlib>
#include <cstdint>
#include <map>
#include <tuple>

typedef uint64_t uint_pr;
typedef std::string str;
typedef float f32;
typedef double f64;

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

template <typename InnerType>
class Enum {
private:
  InnerType* start;
  InnerType* end_;
  uint_pr index = 0;

public:
  explicit Enum(InnerType* s, InnerType* e) : start(s), end_(e) {}
  class Iterator {
  private:
    uint_pr index;
    InnerType* item;

  public:
    Iterator(uint_pr in, InnerType* i) : index(in), item(i) {}
    std::pair<uint_pr, InnerType&> operator*() const { return {index, *item}; }
    Iterator& operator++() { index++; item++; return *this; }
    bool operator!=(const Iterator& i) { return item != i.item; }
  };

  Iterator begin() const { return Iterator(0, start); };
  Iterator end() const { return Iterator(static_cast<uint_pr>(-1), end_); };
};

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

    Enum<T> enumerate() const {
      return Enum(start, start + length);
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
    T data[N];

public:
    explicit Array(std::initializer_list<T> init) {
      std::copy(init.begin(), init.end(), data);
    }

    Enum<T> enumerate() const {
      return Enum<T>((T*)data + 0, (T*)data + N);
    }

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

  void insert(Key k, Value v) {
    contents.insert({k, v});
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

#ifdef MAKE_CHAR
inline char make_char(int num) {
    return char(num);
}
#endif

class BumpAllocator {
private:
  void* memory;
  void* cur;
  uint_pr capacity;

public:
  explicit BumpAllocator(uint_pr cap) {
    capacity = cap;
    memory = malloc(cap);
    cur = memory;
  }

  ~BumpAllocator() {
    if (capacity != 0) {
      deinit();
    }
  }

  void deinit() {
    capacity = 0;
    free(memory);
    std::cout << "cleaned up bump allocator..." << std::endl;
  }

  template<typename Type>
  Option<Type*> allocate(std::initializer_list<Type> init) {
    int type_size = sizeof(Type);
    int align_size = alignof(Type);
    std::size_t available_space = static_cast<std::size_t>((char*)memory + capacity - (char*)cur);

    void* aligned = cur;
    if (std::align(align_size, type_size, aligned, available_space)) {
        if ((char*)aligned + type_size <= (char*)memory + capacity) {
            // Allocation successful
            Type* result = new (aligned) Type(init);  // Placement new for exception safety
            cur = (char*)aligned + type_size;

            std::cout << "allocated: " << type_size << " bytes with an alignment of " << align_size << std::endl;
            std::cout << "used:      " << used() << "/" << capacity << " bytes\n\n";

            return Option<Type*>(result);
        }
    }
    return {};
  }

  template<typename Type>
  Option<Type*> allocate(Type init) {
    int type_size = sizeof(Type);
    int align_size = alignof(Type);
    std::size_t available_space = static_cast<std::size_t>((char*)memory + capacity - (char*)cur);

    void* aligned = cur;
    if (std::align(align_size, type_size, aligned, available_space)) {
        if ((char*)aligned + type_size <= (char*)memory + capacity) {
            // Allocation successful
            Type* result = new (aligned) Type(init);  // Placement new for exception safety
            cur = (char*)aligned + type_size;

            std::cout << "allocated: " << type_size << " bytes with an alignment of " << align_size << std::endl;
            std::cout << "used:      " << used() << "/" << capacity << " bytes\n\n";

            return Option<Type*>(result);
        }
    }
    return {};
  }

  void reset() {
    cur = memory;
  }

  uint_pr used() const {
    return (char *)cur - (char *)memory;
  }

  uint_pr available() const {
    return capacity - used();
  }

  uint_pr cap() const {
    return capacity;
  }
};

template<typename Type>
class PoolBumpAllocator {
private:
  BumpAllocator bump_alo;
  uint_pr num_of_items;

public:
  explicit PoolBumpAllocator<Type>(uint_pr count) :
    num_of_items(count),
    bump_alo(sizeof(Type) * count) {
  }

  ~PoolBumpAllocator() {
    bump_alo.deinit();
  }

  void deinit() {
    bump_alo.deinit();
  }

  Option<Type *> allocate(std::initializer_list<Type> init) {
    return bump_alo.allocate<Type>(init);
  }

  Option<Type *> allocate(Type init) {
    return bump_alo.allocate<Type>(init);
  }

  void reset() {
    bump_alo.reset();
  }

  uint_pr used() const {
    return bump_alo.used();
  }

  uint_pr available() const {
    return bump_alo.available();
  }

  uint_pr cap() const {
    return bump_alo.cap();
  }

  uint_pr num_of_allocatable_items() const {
    return num_of_items;
  }
};

class ArenaAllocator {
};

template<typename IntType>
class Int {
private:
  IntType num;

public:
  Int() : num(0) {}
  Int(IntType n) : num(n) {}

  Int operator+(const Int& n) const {
    return Int(this->num + n.num);
  }

  Int operator*(const Int& n) const  {
    return Int(this->num * n.num);
  }

  Int operator/(const Int& n) const  {
    return Int(this->num / n.num);
  }

  Int operator%(const Int& n) const  {
    return Int(this->num % n.num);
  }

  bool operator>(const Int& n) const {
    return this->num > n.num;
  }

  bool operator<(const Int& n) const {
    return this->num < n.num;
  }

  str as_str() const  {
    return std::to_string(num);
  }
};

template<typename FloatType>
class Float {
private:
  FloatType num;

public:
  Float() : num(0) {}
  Float(FloatType n) : num(n) {}

  template<typename IntType>
  Float(IntType int_n) : num((FloatType) int_n) {}

  Float operator+(const Float& n) const {
    return Float(this->num + n.num);
  }

  Float operator*(const Float& n) const  {
    return Float(this->num * n.num);
  }

  Float operator/(const Float& n) const  {
    return Float(this->num / n.num);
  }

  Float operator%(const Float& n) const  {
    return Float(this->num % n.num);
  }

  str as_str() const  {
    return std::to_string(num);
  }
};

struct Char {
private:
  char c;

public:
  Char() : c('\0') {}
  Char(char ch) : c(ch) {}
  template<typename IntType>
  Char(IntType ascii) : c(char(ascii)) {}

  str operator+(const Char& ch) const {
    return this->as_str() + ch.as_str();
  }

  template<typename IntType>
  str operator*(const Int<IntType>& count) const  {
    str buf = "";
    Int<IntType> iters = 0;
    while (iters < count) {
      buf += c;
      iters = iters + 1;
    }

    return buf;
  }

  str as_str() const  {
    return str(1, c);
  }
};

class Range {
private:
  uint_pr start, end_excl;

public:
  Range(uint_pr s, uint_pr e) : start(s), end_excl(e) {}

  class Iterator {
  private:
    uint_pr value;

  public:
    Iterator(uint_pr val) : value(val) {}
    uint_pr operator*() const { return value; }
    Iterator& operator++() { value++; return *this; }
    bool operator!=(const Iterator& i) { return value != i.value; }
  };

  Iterator begin() const { return Iterator(start); }
  Iterator end() const { return Iterator(end_excl); }
};

template<typename... Types>
class Tuple {
private:
  std::tuple<Types...> data;

public:
  Tuple(Types... args) : data(std::move(args)...) {}

  template<uint_pr Index>
  auto& get() {
    return std::get<Index>(data);
  }

  constexpr static uint_pr len() {
    return sizeof...(Types);
  }

  // Helper function to concatenate strings with a separator
  template<std::size_t... Is>
  str proto_str_helper_tuple(std::index_sequence<Is...>) {
      str content = "(";
      ((content += (Is == 0 ? "" : ", ") + proto_str(std::get<Is>(data))), ...);
      content += ")";
      return content;
  }

  str as_str() {
      return proto_str_helper_tuple(std::make_index_sequence<len()>{});
  }
};
template<typename... Types>
str proto_str(Tuple<Types...>& tuple) {
  return tuple.as_str();
}
