// This function returns an object. Although
// the braces look like Java or C++ they do
// not mean a statement block, they are instead
// the value being returned.
local Sour(spirit, garnish='Lemon twist') = {
  ingredients: [
    { kind: spirit, qty: 2 },
    { kind: 'Egg white', qty: 1 },
    { kind: 'Lemon Juice', qty: 1 },
    { kind: 'Simple Syrup', qty: 1 },
  ],
  garnish: garnish,
  served: 'Straight Up',
};

{
  'Whiskey Sour': Sour('Bulleit Bourbon',
                       'Orange bitters'),
  'Pisco Sour': Sour('Machu Pisco',
                     'Angostura bitters'),
}
