import java.util.ArrayList;

class A {
  void foo() {
    //ERROR: match
    java.util.ArrayList<Integer> a = new java.util.ArrayList<Integer>();
    //TODO: match? but then what is the value for $TYPE?
    java.util.ArrayList<Integer> a = new java.util.ArrayList<>();

    //ERROR: match
    ArrayList<int[]> sudoku = new ArrayList<int[]>();

  }
}
