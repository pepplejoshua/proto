#include <iostream>
#include <algorithm>
#include <initializer_list>
#include <cstdint>
#include <cstdlib>
#include <string>
typedef uint32_t uint;

template<typename T>
class Array {
public:

    Array(std::initializer_list<T> init) {
        for (T item : init) {
            std::cout << item << std::endl;
        }
        std::copy(init.begin(), init.end(), data);
        size = init.size();
    }

    T& operator[](std::size_t index) {
        return data[index];
    }

    const T& operator[](uint index) const {
        return data[index];
    }

    constexpr uint len() const noexcept {
        return size;
    }

    std::string to_string() {

    }

    // Begin and end methods for range-based for loops
    T* begin() noexcept { return data; }
    T* end() noexcept { return data + size; }
    const T* begin() const noexcept { return data; }
    const T* end() const noexcept { return data + size; }
private:
    uint size;
    T data[];
};

void show_array(Array<int> arr) {
    int count = 0;
    std::cout << "[";
    for (int a : arr) {
        std::cout << a;
        count++;
        if (!(count == arr.len())) {
            std::cout << ", ";
        }
    }
    std::cout << "]\n";
}

int main() {
    Array<int> a = {1, 3, 4, 6, 7};
    Array<int> b = {1, 3, 5};
    Array<int> c = {1 };

    show_array(a);
    show_array(b);
    show_array(c);

    return 0;
}
