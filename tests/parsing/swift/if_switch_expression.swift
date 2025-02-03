let foo = if 1 == 1 {
	"foo"
} else {
	"bar"
}

enum X {
	case a
	case b
}

let a = X.a

let bar = switch a {
	case .a:
		"foo"
	case .b:
		"bar"
}
