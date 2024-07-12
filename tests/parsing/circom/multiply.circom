pragma circom 2.1.6;

template Multiply() {
  signal input a;
  signal input b;
  signal output out;

  out <== a * b;
}

component main = Multiply();
