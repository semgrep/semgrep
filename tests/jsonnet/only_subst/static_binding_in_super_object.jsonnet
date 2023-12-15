local A = { a1: 1, a2: self.a1, a3: A.a2 };
A { a1: 2, a2: super.a2 + 1, b1: super.a2, b2: super.a3 }
