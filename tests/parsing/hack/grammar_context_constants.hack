<?hh
abstract class Contexts {
  // Inherited CST ambiguity: an unbounded context is parsed as a typed constant.
  abstract const ctx CEmpty;
  abstract const ctx COne super [defaults];
  abstract const ctx CMany super [defaults] as [io, rand];
  const ctx C = [defaults];
  const ctx CWithBound super [defaults] = [io];
}
