// ERROR: match
if #unavailable(iOS 15) { fallback() }
if #available(iOS 15, *) { modern() }
