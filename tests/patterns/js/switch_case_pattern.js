switch (foo) {
  case 6:
    foo();
    break;
  // MATCH:
  case 5:
    foo();
    break;
  case 7:
    foo();
    break;
  case 8:
  // MATCH:
  case 5:
    foo();
    break;
  default:
    break;
}
