let result = if ready { success() } else { failure() }
let message = switch code { case 0: "ok"; default: "error" }
