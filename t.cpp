#include <cstdlib>
#include <iostream>
#include <string>

typedef std::string str;

void panic(int line, const str sourcefile, const str msg) {
    std::cout << sourcefile << ":" << line << ":" << " " << msg << std::endl;
    std::exit(EXIT_FAILURE);
}

template<typename T>
class Option {
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

template<typename T, std::size_t N>
class Array {
public:
    T data[N] = {};

    Array(std::initializer_list<T> init) {
        std::copy(init.begin(), init.end(), data);
    }

    T& operator[](std::size_t index) {
        return data[index];
    }

    const T& operator[](std::size_t index) const {
        return data[index];
    }

    Option<T> get(std::size_t index) {
        if (index > N-1) {
            return Option<T>();
        }

        return Option<T>(data[index]);
    }

    std::size_t len() const {
        return N;
    }

    // Begin and end methods for range-based for loops
    T* begin() noexcept { return data; }
    T* end() noexcept { return data + N; }
    const T* begin() const noexcept { return data; }
    const T* end() const noexcept { return data + N; }
};

int main() {
    Array<Array<int, 2>, 3> a = {{1, 2}, {3, 4}, {5, 6}};

    for (auto x : a) {
        for (auto y : x) {
            std::cout << y << " ";
        }
        std::cout << std::endl;
    }

    Array<int, 2> *b = &a[0];
    Array<int, 4> *c = new Array<int, 4>({1, 2, 3, 4});
    std::cout << "size of array of 2 ints is " << sizeof(Array<int, 2>) << std::endl;
    Option<int> i = b->get(0);
    Option<int> j = c->get(4);

    // std::cout << i.unwrap() << std::endl;
    // std::cout << j.unwrap() << std::endl;
    std::cout << "sizeof option<int>    :" << sizeof(i) << std::endl;
    std::cout << "sizeof option<int>.tag:" << sizeof(i.tag) << std::endl;
    std::cout << "sizeof int            :" << sizeof(int) << std::endl;

    return 0;
}
