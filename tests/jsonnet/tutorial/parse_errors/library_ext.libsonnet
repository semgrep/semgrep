local fizz = if std.extVar('brunch') then
  'Cheap Sparkling Wine'
else
  'Champagne';
{
  Mimosa: {
    ingredients: [
      { kind: fizz, qty: 3 },
      { kind: 'Orange Juice', qty: 3 },
    ],
    garnish: 'Orange Slice',
    served: 'Champagne Flute',
  },
}
