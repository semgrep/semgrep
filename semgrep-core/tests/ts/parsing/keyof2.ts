export const CellCategoryName = {
  LOGIC: "Logic cells",
  DISPLAY: "Display cells",
  INPUT: "Input parameters",
} as const;
export type CellCategoryName =
    typeof CellCategoryName[keyof typeof CellCategoryName];
