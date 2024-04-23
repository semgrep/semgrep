int main() {

  int bad = 50;
  #ERROR:
  int arr[bad];
  if(1 < 0) {
    #ERROR:
    int arr[bad];
    bad = 500;
    #OK:
    int arr[bad];
  } else {
    bad = 100;
    #OK:
    int arr[bad];
  }
}
