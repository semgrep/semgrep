// ERROR: match
let value = if ready { yes() } else { no() }
let other = ready ? yes() : no()
