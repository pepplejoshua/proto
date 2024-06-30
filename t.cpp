#include <iostream>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef uint64_t pruint;
typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef std::string str;

void panic(int line, const str sourcefile, const str msg) {
    // should unwind the call stack and then show the error message
    std::cout << sourcefile << ":" << line << ":" << " " << msg << std::endl;
    std::exit(EXIT_FAILURE);
}

template <typename T>
inline typename std::enable_if<std::is_arithmetic<T>::value, std::string>::type
proto_str(T value) {
    return std::to_string(value);
}

inline str proto_str(const char c) {
    return str(1, c);
}

inline str proto_str(const bool b) {
    return b ? "true" : "false";
}

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
    // const T* const_start;
    pruint length;
    pruint arr_capacity;

public:
    Slice(T* s, pruint len, pruint cap) : start(s), length(len), arr_capacity(cap) {}

    constexpr pruint len() const noexcept {
        return length;
    }

    T& operator[](pruint index) {
        return start[index];
    }

    const T& operator[](pruint index) const {
        return start[index];
    }

    Option<T> get(pruint index) {
        if (index >= length) {
            return Option<T>();
        }

        return Option<T>(start[index]);
    }

    const Option<T> get(pruint index) const {
        if (index >= length) {
            return Option<T>();
        }

        return Option<T>(start[index]);
    }

    inline Slice<T> make_slice(pruint start, pruint end_exclusive) {
        if (start >= length || end_exclusive > length || start >= end_exclusive) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(this->start + start, end_exclusive - start, arr_capacity);
    }

    inline const Slice<T> make_slice(pruint start, pruint end_exclusive) const {
        if (start >= length || end_exclusive > length || start >= end_exclusive) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(this->start + start, end_exclusive - start, arr_capacity);
    }

    inline Slice<T> make_slice_from(pruint start) {
        if (start >= length) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(this->start + start, length - start, arr_capacity);
    }

    inline const Slice<T> make_slice_from(pruint start) const {
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

template<typename T, pruint N>
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

    const T& operator[](pruint index) const {
        return data[index];
    }

    Option<T> get(pruint index) {
        if (index >= N) {
            return Option<T>();
        }

        return Option<T>(data[index]);
    }

    const Option<T> get(pruint index) const {
        if (index >= N) {
            return Option<T>();
        }

        return Option<T>(data[index]);
    }

    inline Slice<T> make_slice(pruint start, pruint end_exclusive) {
        if (start >= N || end_exclusive > N || start >= end_exclusive) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(data + start, end_exclusive - start, len());
    }

    inline const Slice<T> make_slice(pruint start, pruint end_exclusive) const {
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

    inline Slice<T> make_slice_from(pruint start) {
        if (start >= N) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(data + start, len() - start, len());
    }

    inline const Slice<T> make_slice_from(pruint start) const {
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

    inline constexpr pruint len() const noexcept {
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

template<typename T, pruint N>
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


#include "./src/std/print.cppr"

int main() {
    // const Array<int, 5> a = {1, 3, 4, 6, 7};
    // Array<int, 3> b = {1, 3, 5};

    // const Slice<int> sl = a.make_slice_from(0);
    // const auto sl2 = sl.make_slice_from(0);
    // const auto sl3 = sl2.make_slice(0, 3);
    // auto sl4 = sl2.make_slice(0, 2);
    // const auto sl5 = b.make_slice(0, 2);
    // sl4[1] = 100;
    // u8 u8_num = 69;
    // u64 u64_num = 100000;
    // i64 i64_num = 300000;
    // Option<Array<int, 3>> opt = {};
    // auto opt2 = Option<Slice<int>>(sl3);
    // Array<str, 16> reprs = {
    //     proto_str(sl),
    //     proto_str(sl2),
    //     proto_str(sl3),
    //     proto_str(sl4),
    //     proto_str(sl5),
    //     "",
    //     proto_str(a),
    //     (b[0] = 200, "void"),
    //     proto_str(b),
    //     proto_str(u8_num),
    //     proto_str(u64_num),
    //     proto_str(i64_num),
    //     proto_str(sl.get(1)),
    //     proto_str(sl.get(5)),
    //     proto_str(opt),
    //     proto_str(opt2)
    // };

    // for (str s : reprs) {
    //     proto_println(s);
    // }

    const str a = "hello";
    const str b = "world";

    proto_println(proto_str(a != b));

    return 0;
}
