class Clazz {
    static let a = "hello"
    func something(_ msg: String) {
        switch msg {
            case Self.a: print(msg)
            default: print("not msg")
        }
    }
}

