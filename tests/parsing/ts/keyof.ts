export const VegaFusionEnabled = {
  ALWAYS: "always",
  // In cases where the flag is set to "conditionally", we only want to run a chart through VF
  // iff the cell label contains the string "FusionTime!". This lets us compare VF side-by-side
  // with non-VF in the same project.
  CONDTIONALLY: "conditionally",
  NEVER: "never",
} as const;
export type VegaFusionEnabled =
    typeof VegaFusionEnabled[keyof typeof VegaFusionEnabled];
