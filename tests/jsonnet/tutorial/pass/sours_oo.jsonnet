local templates = import 'templates.libsonnet';

{
  // The template requires us to override
  // the 'spirit'.
  'Whiskey Sour': templates.Sour {
    spirit: 'Whiskey',
  },

  // Specialize it further.
  'Deluxe Sour': self['Whiskey Sour'] {
    // Don't replace the whole sweetner,
    // just change 'kind' within it.
    sweetener+: { kind: 'Gomme Syrup' },
  },

  Daiquiri: templates.Sour {
    spirit: 'Banks 7 Rum',
    citrus+: { kind: 'Lime' },
    // Any field can be overridden.
    garnish: 'Lime wedge',
  },

  "Nor'Easter": templates.Sour {
    spirit: 'Whiskey',
    citrus: { kind: 'Lime', qty: 0.5 },
    sweetener+: { kind: 'Maple Syrup' },
    // +: Can also add to a list.
    ingredients+: [
      { kind: 'Ginger Beer', qty: 1 },
    ],
  },
}
