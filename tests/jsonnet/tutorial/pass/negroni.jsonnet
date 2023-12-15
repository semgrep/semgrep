local utils = import 'utils.libsonnet';
{
  Negroni: {
    // Divide 3oz among the 3 ingredients.
    ingredients: utils.equal_parts(3, [
      'Farmers Gin',
      'Sweet Red Vermouth',
      'Campari',
    ]),
    garnish: 'Orange Peel',
    served: 'On The Rocks',
  },
}
