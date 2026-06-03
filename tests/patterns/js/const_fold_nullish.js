// null ?? value returns value
const x = null ?? "fallback";
//ERROR:
sink(x);

// Non-null left side is kept; must not match sink("fallback")
const y = "kept" ?? "other";
sink(y);
