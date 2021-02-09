import java.util.ArrayList;

class A {
  void foo() {
    //ERROR: match
    java.util.ArrayList<Integer> a = new java.util.ArrayList<>();

    //ERROR: match
    ArrayList<int[]> sudoku = new ArrayList<int[]>();

  }
}
