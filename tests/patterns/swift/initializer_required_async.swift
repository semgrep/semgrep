class Box {
// ERROR: match
required init?(_ value: Int) async throws { use(value) }
init?(_ value: Int) { use(value) }
required init() { setup() }
}
