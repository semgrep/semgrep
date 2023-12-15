{
  // Note that the Mimosa is now
  // parameterized.
  Mimosa(brunch): {
    local fizz = if brunch then
      'Cheap Sparkling Wine'
    else
      'Champagne',
    ingredients: [
      { kind: fizz, qty: 3 },
      { kind: 'Orange Juice', qty: 3 },
    ],
    garnish: 'Orange Slice',
    served: 'Champagne Flute',
  },
}
