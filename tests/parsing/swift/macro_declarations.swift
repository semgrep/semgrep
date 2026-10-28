@freestanding(expression)
public macro stringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "Macros", type: "StringifyMacro")
macro local(_ value: Int) -> Int = #externalMacro(module: "Macros", type: "LocalMacro")
