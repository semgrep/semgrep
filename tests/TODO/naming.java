// should not match since a new 'cond' variable shadows the original one
while (true) {
  if (cond) break;
  boolean cond = true;
  boolean match = cond;
}

while (true) {
  if (cond) break;
  boolean cond = true;
  boolean match = cond;
}

