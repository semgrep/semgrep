local A = { name: 'A' },
      B = { name: 'B', sB: super.name },
      C = { name: 'C', sC: super.name },
      D = { name: 'D', sD: super.name };
(A + B) + (C + D)
