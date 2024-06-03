fn rets() bool {
  // return: bool, a: T_11
  // |> T_11 = int
  a :: 11;

  // return: bool, a: T_11, b: i8
  // |> T_11 = int
  //    T_31 = i8(b)
  b : i8 : 31;

  // return: bool, a: T_11, b: i8, c: T_+
  // |> T_11 = int
  //    T_31 = i8(b)
  //    T_11 = i8(b)
  //    T_+ = T_11
  c :: a + b;

  // return: bool, a: T_11, b: i8, c: T_+, d: T_*
  // |> T_11 = int
  //    T_31 = i8(b)
  //    T_11 = i8(b)
  //    T_+ = T_11
  //    T_2 = int
  //    T_+ = T_2
  //    T_* = T_+
  d :: c * 2;

  // return: bool, a: T_11, b: i8, c: T_+, d: T_*, e: bool
  // |> T_11 = int
  //    T_31 = i8(b)
  //    T_11 = i8(b)
  //    T_+ = T_11
  //    T_2 = int
  //    T_+ = T_2
  //    T_* = T_+
  //    T_10 = int
  //    T_* = T_10
  e :: d > 10;

  // |> T_11 = int
  //    T_31 = i8(b)
  //    T_11 = i8(b)
  //    T_+ = T_11
  //    T_2 = int
  //    T_+ = T_2
  //    T_* = T_+
  //    T_10 = int
  //    T_* = T_10
  //    bool = bool
  return e;
}

fn main() void {    // T_return: void
    res :: rets();  // res: bool
}

fn rets() bool {
  // return: bool, a: T_11
  // |> T_11 = int
  a :: 11;

  // return: bool, a: T_11, b: i8
  // |> T_11 = int
  //    T_31 = i8(b)
  b : i8 : 31;

  // return: bool, a: T_11, b: i8, c: T_+
  // |> T_11 = int
  //    T_31 = i8(b)
  //    T_11 = i8(b)
  //    T_+ = T_11
  c :: a + b;

  // return: bool, a: T_11, b: i8, c: T_+, d: T_*
  // |> T_11 = int
  //    T_31 = i8(b)
  //    T_11 = i8(b)
  //    T_+ = T_11
  //    T_2 = int
  //    T_+ = T_2
  //    T_* = T_+
  d :: c * 2;

  // return: bool, a: T_11, b: i8, c: T_+, d: T_*, e: bool
  // |> T_11 = int
  //    T_31 = i8(b)
  //    T_11 = i8(b)
  //    T_+ = T_11
  //    T_2 = int
  //    T_+ = T_2
  //    T_* = T_+
  //    T_10 = int
  //    T_* = T_10
  e :: d > 10;

  // |> T_11 = int
  //    T_31 = i8(b)
  //    T_11 = i8(b)
  //    T_+ = T_11
  //    T_2 = int
  //    T_+ = T_2
  //    T_* = T_+
  //    T_10 = int
  //    T_* = T_10
  //    bool = bool
  return e;
}
