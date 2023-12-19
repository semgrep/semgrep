// excerpt from std.jsonnet used to debug a bug with
// evaluating format.jsonnet

// Useful utilities
local padding(w, s) =
  local aux(w, v) =
    if w <= 0 then
      v
    else
      aux(w - 1, v + s);
  aux(w, '');

// Add s to the left of str so that its length is at least w.
local pad_left(str, w, s) =
  padding(w - std.length(str), s) + str;

// Render a sign & magnitude integer (radix ranges from decimal to binary).
// neg should be a boolean, and when true indicates that we should render a negative number.
// mag must always be a whole number >= 0, it's the magnitude of the integer to render
// min_chars must be a whole number >= 0
//   It is the field width, i.e. std.length() of the result should be >= min_chars
// min_digits must be a whole number >= 0. It's the number of zeroes to pad with.
// blank must be a boolean, if true adds an additional ' ' in front of a positive number, so
// that it is aligned with negative numbers with the same number of digits.
// plus must be a boolean, if true adds a '+' in front of a positive number, so that it is
// aligned with negative numbers with the same number of digits.  This takes precedence over
// blank, if both are true.
// radix must be a whole number >1 and <= 10.  It is the base of the system of numerals.
// zero_prefix is a string prefixed before the sign to all numbers that are not 0.
local render_int(neg, mag, min_chars, min_digits, blank, plus, radix, zero_prefix) =
  // dec is the minimal string needed to represent the number as text.
  local dec =
    if mag == 0 then
      '0'
    else
      local aux(n) =
        if n == 0 then
          zero_prefix
        else
          aux(std.floor(n / radix)) + (n % radix);
      aux(mag);
  local zp = min_chars - (if neg || blank || plus then 1 else 0);
  local zp2 = std.max(zp, min_digits);
  local dec2 = pad_left(dec, zp2, '0');
  (if neg then '-' else if plus then '+' else if blank then ' ' else '') + dec2;


// found in a trace when debugging format.jsonnet printing "-0" instead of "0"

render_int(true, 0, 0, 0, false, false, 10, "")
