#include <iostream>
#include <algorithm>
#include <initializer_list>
#include <cstdint>
#include <cstdlib>
#include <string>
typedef uint32_t uint;
typedef std::string str;

void panic(int line, const str sourcefile, const str msg) {
    // should unwind the call stack and then show the error message
    std::cout << sourcefile << ":" << line << ":" << " " << msg << std::endl;
    std::exit(EXIT_FAILURE);
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

    Option(T item) {
        data = item;
        tag = Some;
    }

    Option() {
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
            panic(__LINE__, __FILE__, "attempted to unwrap an Option::None.");
        }
        return data;
    }

    inline const T& unwrap() const {
        // should use a panic() function to fail if Option.is_none() is true
        if (this->is_none()) {
            panic(__LINE__, __FILE__, "attempted to unwrap an Option::None.");
        }
        return data;
    }
};

template<typename T>
class Slice {
private:
    T* start;
    // const T* const_start;
    uint length;
    uint arr_capacity;

public:
    Slice(T* s, uint len, uint cap) : start(s), length(len), arr_capacity(cap) {}

    constexpr uint len() const noexcept {
        return length;
    }

    T& operator[](uint index) {
        return start[index];
    }

    const T& operator[](uint index) const {
        return start[index];
    }

    Option<T> get(uint index) {
        if (index >= length) {
            return Option<T>();
        }

        return Option<T>(this[index]);
    }

    const Option<T> get(uint index) const {
        if (index >= length) {
            return Option<T>();
        }

        return Option<T>(this[index]);
    }

    inline Slice<T> make_slice(uint start, uint end_exclusive) {
        if (start >= length || end_exclusive > length || start >= end_exclusive) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(this->start + start, end_exclusive - start, arr_capacity);
    }

    inline const Slice<T> make_slice(uint start, uint end_exclusive) const {
        if (start >= length || end_exclusive > length || start >= end_exclusive) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(this->start + start, end_exclusive - start, arr_capacity);
    }

    inline Slice<T> make_slice_from(uint start) {
        if (start >= length) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(this->start + start, length - start, arr_capacity);
    }

    inline const Slice<T> make_slice_from(uint start) const {
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


template<typename T, uint N>
class Array {
private:
    T data[N] = {};

public:
    Array(std::initializer_list<T> init) {
        std::copy(init.begin(), init.end(), data);
    }

    T& operator[](std::size_t index) {
        return data[index];
    }

    const T& operator[](uint index) const {
        return data[index];
    }

    Option<T> get(uint index) {
        if (index >= N) {
            return Option<T>();
        }

        return Option<T>(data[index]);
    }

    const Option<T> get(uint index) const {
        if (index >= N) {
            return Option<T>();
        }

        return Option<T>(this[index]);
    }

    inline Slice<T> make_slice(uint start, uint end_exclusive) {
        if (start >= N || end_exclusive > N || start >= end_exclusive) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(data + start, end_exclusive - start, len());
    }

    inline const Slice<T> make_slice(uint start, uint end_exclusive) const {
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

    inline Slice<T> make_slice_from(uint start) {
        if (start >= N) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(data + start, len() - start, len());
    }

    inline const Slice<T> make_slice_from(uint start) const {
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

    inline constexpr uint len() const noexcept {
        return N;
    }

    // Begin and end methods for range-based for loops
    inline T* begin() noexcept { return data; }
    inline T* end() noexcept { return data + N; }
    inline const T* begin() const noexcept { return data; }
    inline const T* end() const noexcept { return data + N; }
};

template<typename T>
inline void proto_show(const Slice<T> slice) {
    int count = 0;
    std::cout << "[";
    for (int a : slice) {
        std::cout << a;
        count++;
        if (!(count == slice.len())) {
            std::cout << ", ";
        }
    }
    std::cout << "]\n";
}

template<typename T, uint N>
inline void proto_show(const Array<T, N> arr) {
    int count = 0;
    std::cout << "[";
    for (int a : arr) {
        std::cout << a;
        count++;
        if (!(count == N)) {
            std::cout << ", ";
        }
    }
    std::cout << "]\n";
}

int main() {
    const Array<int, 5> a = {1, 3, 4, 6, 7};
    Array<int, 3> b = {1, 3, 5};

    const Slice<int> sl = a.make_slice_from(0);
    const auto sl2 = sl.make_slice_from(0);
    const auto sl3 = sl2.make_slice(0, 3);
    auto sl4 = sl2.make_slice(0, 2);
    const auto sl5 = b.make_slice(0, 2);
    sl4[1] = 100;
    proto_show(sl);
    proto_show(sl2);
    proto_show(sl3);
    proto_show(sl4);
    proto_show(sl5);
    std::cout << std::endl;

    proto_show(a);
    proto_show(b);
    return 0;
}
