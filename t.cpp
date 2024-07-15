#include "t.h"

using std::cout;
using std::endl;

struct Pair {
  char key; int count;

  Pair(char k, int c) {
    key = k;
    count = c;
  }
};

str proto_str(const Pair p) {
  str s = "(";
  s += p.key;
  s += ", ";
  s += proto_str(p.count);
  s += ")";
  return s;
}

Slice<Pair> compress(str og) {
  auto compressed = Slice<Pair>();
  int count = 1;

  for (int i = 1; i < og.size(); ++i) {
    if (og[i] == og[i-1]) {
      count += 1;
    } else {
      compressed.append(Pair(og[i - 1], count));
      count = 1;
    }
  }
  compressed.append(Pair(og[og.size() - 1], count));
  return compressed;
}

str decompress(Slice<Pair> compressed) {
  str og = "";
  for (auto p : compressed) {
    og += str(p.count, p.key);
  }

  return og;
}

int main() {
  str og = "AABBBCCCC";
  auto compressed = compress(og);
  cout << "compressed: " << proto_str(compressed) << endl;

  str decompressed = decompress(compressed);
  cout << "decompressed: " << decompressed << endl;

  if (og == decompressed) {
    cout << "compression successful!" << endl;
  } else {
    cout << "compression failed!" << endl;
  }

  return 0;
}
