// ERROR: match
if !flag { other() }
// Must not match: #unavailable is not a Not of the platform args.
if #unavailable(iOS 15) { fallback() }
if #available(iOS 15, *) { modern() }
