// ERROR: match
if #available(iOS 15, *) { modern() }
if #unavailable(iOS 15) { fallback() }
if !flag { other() }
