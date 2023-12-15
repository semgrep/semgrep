local sours = import 'sours_oo.jsonnet';

local RemoveGarnish = {
  // Not technically removed, but made hidden.
  garnish:: super.garnish,
};

// Make virgin cocktails
local NoAlcohol = {
  local Substitute(ingredient) =
    local k = ingredient.kind;
    local bitters = 'Angustura Bitters';
    if k == 'Whiskey' then [
      { kind: 'Water', qty: ingredient.qty },
      { kind: bitters, qty: 'tsp' },
    ] else if k == 'Banks 7 Rum' then [
      { kind: 'Water', qty: ingredient.qty },
      { kind: 'Vanilla Essence', qty: 'dash' },
      { kind: bitters, qty: 'dash' },
    ] else [
      ingredient,
    ],
  ingredients: std.flattenArrays([
    Substitute(i)
    for i in super.ingredients
  ]),
};

local PartyMode = {
  served: 'In a plastic cup',
};

{
  'Whiskey Sour':
    sours['Whiskey Sour']
    + RemoveGarnish + PartyMode,

  'Virgin Whiskey Sour':
    sours['Whiskey Sour'] + NoAlcohol,

  'Virgin Daiquiri':
    sours.Daiquiri + NoAlcohol,

}
