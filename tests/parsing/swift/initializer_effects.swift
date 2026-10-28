class Box {
    required init?(_ value: Int) async throws { use(value) }
    init!() { setup() }
}
protocol Factory { init() async throws }
