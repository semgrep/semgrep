{
  Martini: {
    local drink = self,
    ingredients: [
      { kind: "Farmer's Gin", qty: 1 },
      {
        kind: 'Dry White Vermouth',
        qty: drink.ingredients[0].qty,
      },
    ],
    garnish: 'Olive',
    served: 'Straight Up',
  },
}
