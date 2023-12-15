local lib = import 'library_tla.libsonnet';

// Here is the top-level function, note brunch
// now has a default value.
function(prefix, brunch=false) {

  [prefix + 'Pina Colada']: {
    ingredients: [
      { kind: 'Rum', qty: 3 },
      { kind: 'Pineapple Juice', qty: 6 },
      { kind: 'Coconut Cream', qty: 2 },
      { kind: 'Ice', qty: 12 },
    ],
    garnish: 'Pineapple slice',
    served: 'Frozen',
  },

  [if brunch then prefix + 'Bloody Mary']: {
    ingredients: [
      { kind: 'Vodka', qty: 1.5 },
      { kind: 'Tomato Juice', qty: 3 },
      { kind: 'Lemon Juice', qty: 1.5 },
      { kind: 'Worcestershire', qty: 0.25 },
      { kind: 'Tobasco Sauce', qty: 0.15 },
    ],
    garnish: 'Celery salt & pepper',
    served: 'Tall',
  },

  [prefix + 'Mimosa']: lib.Mimosa(brunch),
}
