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
    const T* const_start;
    uint length;
    uint arr_capacity;

public:
    Slice(T* s, uint len, uint cap) : start(s), const_start(s), length(len), arr_capacity(cap) {}
    Slice(const T* s, uint len, uint cap) : start(nullptr), const_start(s), length(len), arr_capacity(cap) {}
    Slice(const Slice<T>& other) :
                const_start(other.const_start ? other.const_start : other.start),
                length(other.length), arr_capacity(other.arr_capacity) {}
    Slice(Slice<T>& other) :
                start(other.start),
                const_start(nullptr),
                length(other.length), arr_capacity(other.arr_capacity) {}

    constexpr uint len() const noexcept {
        return length;
    }

    T& operator[](uint index) {
        return *(start + index);
    }

    const T& operator[](uint index) const {
        return *((start ? start : const_start) + index);
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

    Slice<T> make_slice(uint start, uint end_exclusive) {
        if (start >= length || end_exclusive > length || start >= end_exclusive) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(this->start + start, end_exclusive - start, arr_capacity);
    }

    const Slice<T> make_slice(uint start, uint end_exclusive) const {
        if (start >= length || end_exclusive > length || start >= end_exclusive) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>((this->start ? this->start : this->const_start) + start, end_exclusive - start, arr_capacity);
    }

    Slice<T> make_slice_from(uint start) {
        if (start >= length) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(this->start + start, length - start, arr_capacity);
    }

    const Slice<T> make_slice_from(uint start) const {
        if (start >= length) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>((this->start ? this->start : this->const_start) + start, length - start, arr_capacity);
    }

    // Begin and end methods for range-based for loops
    T* begin() noexcept { return start; }
    T* end() noexcept { return start + length; }
    const T* begin() const noexcept { return (start ? start : const_start); }
    const T* end() const noexcept { return (start ? start : const_start) + length; }
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

    Slice<T> make_slice(uint start, uint end_exclusive) {
        if (start >= N || end_exclusive > N || start >= end_exclusive) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(this->begin() + start, end_exclusive - start, len());
    }

    const Slice<T> make_slice(uint start, uint end_exclusive) const {
        if (start >= N || end_exclusive > N || start >= end_exclusive) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(this->begin() + start, end_exclusive - start, len());
    }

    Slice<T> make_slice_from(uint start) {
        if (start >= N) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(this->begin() + start, len() - start, len());
    }

    const Slice<T> make_slice_from(uint start) const {
        if (start >= N) {
            panic(__LINE__, __FILE__, "Invalid slice bounds");
        }
        return Slice<T>(this->begin() + start, len() - start, len());
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

void mod_slice(Slice<int> slice) {
    slice[0] = 100;
}

void mod_slice1(Slice<int>& slice) {
    slice[0] = 100;
}

int main() {
    Array<int, 5> a = {1, 3, 4, 6, 7};
    Array<int, 3> b = {1, 3, 5};

    // todo: prevent assigning a constant to a non-constant
    // reeiver will stop the bug where:
    // Slice<int> sl = a.make_slice_from(2);
    // this will segfault because
    Slice<int> sl = a.make_slice_from(2);
    show_slice(sl);
    return 0;
}
