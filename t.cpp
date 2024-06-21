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

#include "./src/std/to_string.cppr"
#include "./src/std/option.cppr"
#include "./src/std/slice_and_array.cppr"
#include "./src/std/print.cppr"

int main() {
    const Array<int, 5> a = {1, 3, 4, 6, 7};
    Array<int, 3> b = {1, 3, 5};

    const Slice<int> sl = a.make_slice_from(0);
    const auto sl2 = sl.make_slice_from(0);
    const auto sl3 = sl2.make_slice(0, 3);
    auto sl4 = sl2.make_slice(0, 2);
    const auto sl5 = b.make_slice(0, 2);
    sl4[1] = 100;
    u8 u8_num = 69;
    u64 u64_num = 100000;
    i64 i64_num = 300000;
    Option<Array<int, 3>> opt = Option<Array<int, 3>>(b);
    auto opt2 = Option<Slice<int>>(sl3);
    Array<str, 16> reprs = {
        proto_str(sl),
        proto_str(sl2),
        proto_str(sl3),
        proto_str(sl4),
        proto_str(sl5),
        "",
        proto_str(a),
        (b[0] = 200, "void"),
        proto_str(b),
        proto_str(u8_num),
        proto_str(u64_num),
        proto_str(i64_num),
        proto_str(sl.get(1)),
        proto_str(sl.get(5)),
        proto_str(opt),
        proto_str(opt2)
    };

    for (str s : reprs) {
        proto_println(s);
    }

    return 0;
}
