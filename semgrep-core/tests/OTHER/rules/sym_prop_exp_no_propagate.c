
int x = 2;

int y = g(x);

x = 3;

int main() {
  // No finding here, because `x` is not known to be assigned to once!
  int z = f(y)
}