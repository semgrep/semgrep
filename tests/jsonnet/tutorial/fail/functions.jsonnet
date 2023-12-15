// Define a local function.
// Default arguments are like Python:
local my_function(x, y=10) = x + y;

// Define a local multiline function.
local multiline_function(x) =
  // One can nest locals.
  local temp = x * 2;
  // Every local ends with a semi-colon.
  [temp, temp + 1];

local object = {
  // A method
  my_method(x): x * x,
};

{
  // Functions are first class citizens.
  call_inline_function:
    (function(x) x * x)(5),

  call_multiline_function: multiline_function(4),

  // Using the variable fetches the function,
  // the parens call the function.
  call: my_function(2),

  // Like python, parameters can be named at
  // call time.
  named_params: my_function(x=2),
  // This allows changing their order
  named_params2: my_function(y=3, x=2),

  // object.my_method returns the function,
  // which is then called like any other.
  call_method1: object.my_method(3),

  standard_lib:
    std.join(' ', std.split('foo/bar', '/')),
  len: [
    std.length('hello'),
    std.length([1, 2, 3]),
  ],
}
