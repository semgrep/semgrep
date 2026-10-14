<?hh
// ERROR: match
abstract class Yes { const ctx C = [io]; }
abstract class No { const ctx C = [rand]; }
abstract class Bound { abstract const ctx C super [io]; }
