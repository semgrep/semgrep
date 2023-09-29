public class A {
  int main() {
    String s = "semgrep";

    // ERROR: match
    int it = STR."i love semgrep!";
    // ERROR: match
    int it = STR."i love \{s}!";

    int not_it = STR."i hate \{s}";
    int not_it2 = STR."love \{s}!";
    int not_it3 = STR."i love \{s}";
  }
}