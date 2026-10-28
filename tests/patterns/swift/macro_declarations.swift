// ERROR: match
macro local(_ value: Int) -> Int = #externalMacro(module: "Macros", type: "LocalMacro")
macro other(_ value: String) -> String = #externalMacro(module: "Macros", type: "OtherMacro")
