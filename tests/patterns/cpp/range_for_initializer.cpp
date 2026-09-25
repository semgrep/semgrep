void loops() {
  //ERROR: match
  for (int init; int value : values) { consume(value); }
  for (int init; int value : values) { ignore(value); }
}
