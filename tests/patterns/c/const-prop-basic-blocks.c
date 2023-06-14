int loop() {
  int x = 0;
  while(false) {
    x = 1;
  }
  #OK:
  return x;
}

int seth() {
  int x = 0;
  int y = x;
  #ERROR:
  return y;
}

int main() {
  int x = 0;

  if(x) {
    #ERROR:
    return x;
  } else {
    #ERROR:
    return x;
  }
 
  /* dead code! */
  // however we should still match syn. equiv
  #ERROR:
  return 0; 

  while(x) {
    #OK:
    return x;
  }

  #OK:
  return x;
}
