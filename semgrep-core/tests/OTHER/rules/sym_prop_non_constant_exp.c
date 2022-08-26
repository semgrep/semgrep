
int x = 2;

int y = g(x);

// ruleid: find-propagated-non-constant-exp 
int h = f(y);

x = 3;

int main() {
  // ruleid: find-propagated-non-constant-exp 
  int z = f(y)
}