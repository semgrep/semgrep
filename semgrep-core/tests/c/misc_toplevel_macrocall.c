//ERROR: match,  this fails to parse with pfff
SOME_FUNC(1);

// This is OK
int main() {
  //ERROR: match
  SOME_FUNC(1);
}
