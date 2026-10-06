void main() {
  const bad = "password";

  //ERROR: match
  dangerous1("password");

  //ERROR: match
  dangerous2(bad);
}
