class x {
  public void main() {
    char c = 'a';

    // ERROR: match
    int i = switch (c) {
        case 'a' -> 420;
    };
  }
}
