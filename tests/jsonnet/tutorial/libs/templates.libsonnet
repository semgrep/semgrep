{
  // Abstract template of a "sour" cocktail.
  Sour: {
    local drink = self,

    // Hidden fields can be referred to
    // and overridden, but do not appear
    // in the JSON output.
    citrus:: {
      kind: 'Lemon Juice',
      qty: 1,
    },
    sweetener:: {
      kind: 'Simple Syrup',
      qty: 0.5,
    },

    // A field that must be overridden.
    spirit:: error 'Must override "spirit"',

    ingredients: [
      { kind: drink.spirit, qty: 2 },
      drink.citrus,
      drink.sweetener,
    ],
    garnish: self.citrus.kind + ' twist',
    served: 'Straight Up',
  },
}
