#include <cstdint>
#include <cstdio>
#include <cstdlib>

using i8 = std::int8_t;
using i16 = std::int16_t;
using i32 = std::int32_t;
using i64 = std::int64_t;
using isize = std::intptr_t;

using u8 = std::uint8_t;
using u16 = std::uint16_t;
using u32 = std::uint32_t;
using u64 = std::uint64_t;
using usize = std::uintptr_t;

constexpr i8 add_i8(i8 a, i8 b) { return a + b; }
constexpr i16 add_i16(i16 a, i16 b) { return a + b; }
constexpr i32 add_i32(i32 a, i32 b) { return a + b; }
constexpr i64 add_i64(i64 a, i64 b) { return a + b; }
constexpr isize add_isize(isize a, isize b) { return a + b; }
constexpr u8 add_u8(u8 a, u8 b) { return a + b; }
constexpr u16 add_u16(u16 a, u16 b) { return a + b; }
constexpr u32 add_u32(u32 a, u32 b) { return a + b; }
constexpr u64 add_u64(u64 a, u64 b) { return a + b; }
constexpr usize add_usize(usize a, usize b) { return a + b; }

constexpr i8 sub_i8(i8 a, i8 b) { return a - b; }
constexpr i16 sub_i16(i16 a, i16 b) { return a - b; }
constexpr i32 sub_i32(i32 a, i32 b) { return a - b; }
constexpr i64 sub_i64(i64 a, i64 b) { return a - b; }
constexpr isize sub_isize(isize a, isize b) { return a - b; }
constexpr u8 sub_u8(u8 a, u8 b) { return a - b; }
constexpr u16 sub_u16(u16 a, u16 b) { return a - b; }
constexpr u32 sub_u32(u32 a, u32 b) { return a - b; }
constexpr u64 sub_u64(u64 a, u64 b) { return a - b; }
constexpr usize sub_usize(usize a, usize b) { return a - b; }

constexpr i8 mul_i8(i8 a, i8 b) { return a * b; }
constexpr i16 mul_i16(i16 a, i16 b) { return a * b; }
constexpr i32 mul_i32(i32 a, i32 b) { return a * b; }
constexpr i64 mul_i64(i64 a, i64 b) { return a * b; }
constexpr isize mul_isize(isize a, isize b) { return a * b; }
constexpr u8 mul_u8(u8 a, u8 b) { return a * b; }
constexpr u16 mul_u16(u16 a, u16 b) { return a * b; }
constexpr u32 mul_u32(u32 a, u32 b) { return a * b; }
constexpr u64 mul_u64(u64 a, u64 b) { return a * b; }
constexpr usize mul_usize(usize a, usize b) { return a * b; }

constexpr i8 div_i8(i8 a, i8 b) { return a / b; }
constexpr i16 div_i16(i16 a, i16 b) { return a / b; }
constexpr i32 div_i32(i32 a, i32 b) { return a / b; }
constexpr i64 div_i64(i64 a, i64 b) { return a / b; }
constexpr isize div_isize(isize a, isize b) { return a / b; }
constexpr u8 div_u8(u8 a, u8 b) { return a / b; }
constexpr u16 div_u16(u16 a, u16 b) { return a / b; }
constexpr u32 div_u32(u32 a, u32 b) { return a / b; }
constexpr u64 div_u64(u64 a, u64 b) { return a / b; }
constexpr usize div_usize(usize a, usize b) { return a / b; }

constexpr i8 rem_i8(i8 a, i8 b) { return a % b; }
constexpr i16 rem_i16(i16 a, i16 b) { return a % b; }
constexpr i32 rem_i32(i32 a, i32 b) { return a % b; }
constexpr i64 rem_i64(i64 a, i64 b) { return a % b; }
constexpr isize rem_isize(isize a, isize b) { return a % b; }
constexpr u8 rem_u8(u8 a, u8 b) { return a % b; }
constexpr u16 rem_u16(u16 a, u16 b) { return a % b; }
constexpr u32 rem_u32(u32 a, u32 b) { return a % b; }
constexpr u64 rem_u64(u64 a, u64 b) { return a % b; }
constexpr usize rem_usize(usize a, usize b) { return a % b; }

constexpr i8 neg_i8(i8 a) { return -a; }
constexpr i16 neg_i16(i16 a) { return -a; }
constexpr i32 neg_i32(i32 a) { return -a; }
constexpr i64 neg_i64(i64 a) { return -a; }
constexpr isize neg_isize(isize a) { return -a; }

constexpr bool lt_i8(i8 a, i8 b) { return a < b; }
constexpr bool lt_i16(i16 a, i16 b) { return a < b; }
constexpr bool lt_i32(i32 a, i32 b) { return a < b; }
constexpr bool lt_i64(i64 a, i64 b) { return a < b; }
constexpr bool lt_isize(isize a, isize b) { return a < b; }
constexpr bool lt_u8(u8 a, u8 b) { return a < b; }
constexpr bool lt_u16(u16 a, u16 b) { return a < b; }
constexpr bool lt_u32(u32 a, u32 b) { return a < b; }
constexpr bool lt_u64(u64 a, u64 b) { return a < b; }
constexpr bool lt_usize(usize a, usize b) { return a < b; }

constexpr bool gt_i8(i8 a, i8 b) { return a > b; }
constexpr bool gt_i16(i16 a, i16 b) { return a > b; }
constexpr bool gt_i32(i32 a, i32 b) { return a > b; }
constexpr bool gt_i64(i64 a, i64 b) { return a > b; }
constexpr bool gt_isize(isize a, isize b) { return a > b; }
constexpr bool gt_u8(u8 a, u8 b) { return a > b; }
constexpr bool gt_u16(u16 a, u16 b) { return a > b; }
constexpr bool gt_u32(u32 a, u32 b) { return a > b; }
constexpr bool gt_u64(u64 a, u64 b) { return a > b; }
constexpr bool gt_usize(usize a, usize b) { return a > b; }

constexpr bool lte_i8(i8 a, i8 b) { return a <= b; }
constexpr bool lte_i16(i16 a, i16 b) { return a <= b; }
constexpr bool lte_i32(i32 a, i32 b) { return a <= b; }
constexpr bool lte_i64(i64 a, i64 b) { return a <= b; }
constexpr bool lte_isize(isize a, isize b) { return a <= b; }
constexpr bool lte_u8(u8 a, u8 b) { return a <= b; }
constexpr bool lte_u16(u16 a, u16 b) { return a <= b; }
constexpr bool lte_u32(u32 a, u32 b) { return a <= b; }
constexpr bool lte_u64(u64 a, u64 b) { return a <= b; }
constexpr bool lte_usize(usize a, usize b) { return a <= b; }

constexpr bool gte_i8(i8 a, i8 b) { return a >= b; }
constexpr bool gte_i16(i16 a, i16 b) { return a >= b; }
constexpr bool gte_i32(i32 a, i32 b) { return a >= b; }
constexpr bool gte_i64(i64 a, i64 b) { return a >= b; }
constexpr bool gte_isize(isize a, isize b) { return a >= b; }
constexpr bool gte_u8(u8 a, u8 b) { return a >= b; }
constexpr bool gte_u16(u16 a, u16 b) { return a >= b; }
constexpr bool gte_u32(u32 a, u32 b) { return a >= b; }
constexpr bool gte_u64(u64 a, u64 b) { return a >= b; }
constexpr bool gte_usize(usize a, usize b) { return a >= b; }

constexpr bool eq_i8(i8 a, i8 b) { return a == b; }
constexpr bool eq_i16(i16 a, i16 b) { return a == b; }
constexpr bool eq_i32(i32 a, i32 b) { return a == b; }
constexpr bool eq_i64(i64 a, i64 b) { return a == b; }
constexpr bool eq_isize(isize a, isize b) { return a == b; }
constexpr bool eq_u8(u8 a, u8 b) { return a == b; }
constexpr bool eq_u16(u16 a, u16 b) { return a == b; }
constexpr bool eq_u32(u32 a, u32 b) { return a == b; }
constexpr bool eq_u64(u64 a, u64 b) { return a == b; }
constexpr bool eq_usize(usize a, usize b) { return a == b; }

constexpr bool neq_i8(i8 a, i8 b) { return a != b; }
constexpr bool neq_i16(i16 a, i16 b) { return a != b; }
constexpr bool neq_i32(i32 a, i32 b) { return a != b; }
constexpr bool neq_i64(i64 a, i64 b) { return a != b; }
constexpr bool neq_isize(isize a, isize b) { return a != b; }
constexpr bool neq_u8(u8 a, u8 b) { return a != b; }
constexpr bool neq_u16(u16 a, u16 b) { return a != b; }
constexpr bool neq_u32(u32 a, u32 b) { return a != b; }
constexpr bool neq_u64(u64 a, u64 b) { return a != b; }
constexpr bool neq_usize(usize a, usize b) { return a != b; }

struct str {
  const char *data;
  usize len;

  constexpr str(const char *data, usize len) : data(data), len(len) {}
  constexpr str(const char *data) : data(data), len(0) {
    while (data[len] != '\0') {
      len++;
    }
  }

  constexpr char operator[](usize i) const { return data[i]; }
};

template <typename T> struct Slice {
  T *data;
  usize len;
  usize cap;

  constexpr Slice() : data(nullptr), len(0), cap(0) {}
  constexpr Slice(T *data, usize len, usize cap)
      : data(data), len(len), cap(cap) {}
  constexpr Slice(T *data, usize len) : data(data), len(len), cap(len) {}
  constexpr Slice(T *data) : data(data), len(0), cap(0) {}

  constexpr T &operator[](usize i) { return data[i]; }
  constexpr const T &operator[](usize i) const { return data[i]; }
};

template <typename T> struct Vec {
  T *data;
  usize len;
  usize cap;

  constexpr Vec() : data(nullptr), len(0), cap(0) {}
  constexpr Vec(T *data, usize len, usize cap)
      : data(data), len(len), cap(cap) {}
  constexpr Vec(T *data, usize len) : data(data), len(len), cap(len) {}
  constexpr Vec(T *data) : data(data), len(0), cap(0) {}

  constexpr T &operator[](usize i) { return data[i]; }
  constexpr const T &operator[](usize i) const { return data[i]; }
  constexpr void push(T t) {
    if (len == cap) {
      cap = cap * 2 + 1;
      T *new_data = new T[cap];
      for (usize i = 0; i < len; i++) {
        new_data[i] = data[i];
      }
      delete[] data;
      data = new_data;
    }
    data[len++] = t;
  }

  constexpr T pop() { return data[--len]; }
};

template <typename T> struct Box {
  T *data;

  constexpr Box() : data(nullptr) {}
  constexpr Box(T *data) : data(data) {}

  constexpr T &operator*() { return *data; }
  constexpr const T &operator*() const { return *data; }
  constexpr T *operator->() { return data; }
  constexpr const T *operator->() const { return data; }
};

constexpr bool operator==(str a, str b) {
  if (a.len != b.len) {
    return false;
  }
  for (usize i = 0; i < a.len; i++) {
    if (a[i] != b[i]) {
      return false;
    }
  }
  return true;
}

static void panic(const char *msg) {
  std::printf("panic: %s\n", msg);
  exit(1);
}

template <typename T> struct Option {
  T some;
  bool none = false;

  constexpr Option() : none(true) {}
  constexpr Option(T some) : some(some), none(false) {}

  constexpr bool is_some() const { return !none; }
  constexpr bool is_none() const { return none; }
  constexpr T unwrap() const {
    if (is_none()) {
      panic("called `Option::unwrap()` on a `None` value");
    }
    return some;
  }

  constexpr T unwrap_or(T def) const {
    if (is_none()) {
      return def;
    }
    return some;
  }

  constexpr T unwrap_or_else(T (*f)()) const {
    if (is_none()) {
      return f();
    }
    return some;
  }

  constexpr T unwrap_or_else(T (*f)(void *), void *data) const {
    if (is_none()) {
      return f(data);
    }
    return some;
  }
};
