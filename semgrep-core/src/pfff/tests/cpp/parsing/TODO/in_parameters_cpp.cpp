void main() {
  try {
  }
  // this must be inferred as a typedef, and so is also kind of InParameter
  catch (foo10& x) {
  }
}


Foo::Foo(foo10* x) {
}

//TODO
//ostream& operator<<(ostream &ostream, Node* node) {
//}


struct PerUserLimitCacheKey
{
  friend std::ostream& operator<<(std::ostream& os,
                                   const PerUserLimitCacheKey& k);
};


class Util {
  static int64_t normalizeTimestamp(int64_t timestamp, int timeUnit = 60);
};
