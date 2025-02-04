enum A {
	case x
	case y
}

let x = A.x
// MATCH:
let foo = switch x {
case .x: 1
case .y: 2
}
