<?hh
// ERROR: match
abstract class Lower { abstract const ctx C super [io]; }
abstract class Upper { abstract const ctx C as [io]; }
abstract class Other { abstract const ctx C super [rand]; }
