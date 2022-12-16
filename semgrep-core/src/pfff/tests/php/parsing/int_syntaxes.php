<?hh

function octal(): int {
  return 0170;
}

function binary(): int {
  return 0b101 ^ 0b011;
}

function hex(): int {
  return 0x7F + 0XA9;
}
