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
class Slice;

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
        if (index > N-1) {
            return Option<T>();
        }

        return Option<T>(data[index]);
    }

    Slice<T> make_slice(uint start, uint end_exclusive) {
        return Slice<T>(this->begin() + start, end_exclusive-start, len());
    }

    constexpr uint len() const noexcept {
        return N;
    }

    // Begin and end methods for range-based for loops
    T* begin() noexcept { return data; }
    T* end() noexcept { return data + N; }
    const T* begin() const noexcept { return data; }
    const T* end() const noexcept { return data + N; }
};

template<typename T>
class Slice {
private:
    T* start;
    uint length;
    uint arr_capacity;

public:
    Slice(T* s, uint len, uint cap) : start(s), length(len), arr_capacity(cap) {}

    constexpr uint len() const noexcept {
        return length;
    }

    T& operator[](uint index) {
        return start + index;
    }

    const T& operator[](uint index) const {
        return start + index;
    }

    Option<T> get(uint index) {
        if (index > length-1) {
            return Option<T>();
        }

        return Option<T>(this[index]);
    }

    // Begin and end methods for range-based for loops
    T* begin() noexcept { return start; }
    T* end() noexcept { return start + length; }
    const T* begin() const noexcept { return start; }
    const T* end() const noexcept { return start + length; }
};

void show_slice(Slice<int> slice) {
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

int main() {
    Array<int, 5> a = {1, 3, 4, 6, 7};
    Array<int, 3> b = {1, 3, 5};

    show_slice(b.make_slice(0, 3));

    return 0;
}
