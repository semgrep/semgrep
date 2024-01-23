local Mojito(virgin=false, large=false) = {
  // A local next to fields ends with ','.
  local factor = if large then 2 else 1,
  // The ingredients are split into 3 arrays,
  // the middle one is either length 1 or 0.
  ingredients: [
    {
      kind: 'Mint',
      action: 'muddle',
      qty: 6 * factor,
      unit: 'leaves',
    },
  ] + (
    if virgin then [] else [
      { kind: 'Banks', qty: 1.5 * factor },
    ]
  ) + [
    { kind: 'Lime', qty: 0.5 * factor },
    { kind: 'Simple Syrup', qty: 0.5 * factor },
    { kind: 'Soda', qty: 3 * factor },
  ],
  // Returns null if not large.
  garnish: if large then 'Lime wedge',
  served: 'Over crushed ice',
};

{
  Mojito: Mojito(),
  'Virgin Mojito': Mojito(virgin=true),
  'Large Mojito': Mojito(large=true),
}
