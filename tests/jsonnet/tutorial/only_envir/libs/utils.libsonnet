{
  equal_parts(size, ingredients)::
    // Define a function-scoped variable.
    local qty = size / std.length(ingredients);
    // Return an array.
    [
      { kind: i, qty: qty }
      for i in ingredients
    ],
}
