
// Yes, I copied this from w3schools from searching up "C++ classes",
// as the first result.
// TODO?
class MyClass {
  public:
    int myNum;
    string myString;

    // ERROR: match
    double doSomething(){
        return something;
    }
};

// ERROR: match
int main() {
  int x = 2;
  int y = 3;
}