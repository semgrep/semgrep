package p

// ruleid: go-type-switch-case
var _ = a.b

func f(x any, v any) {
// ok: go-type-switch-case
	var y a.b
	switch x {
// ruleid: go-type-switch-case
	case a.b:
// ok: go-type-switch-case
	case c.d:
	}

	// ruleid: go-type-switch-stmt
	switch t := v.(type) {
	// ruleid: go-type-switch-case
	case a.b:
		_ = t
	// ok: go-type-switch-case
	case c.d:
	// ok: go-type-switch-case
	case *a.b:
	// ok: go-type-switch-case
	case int:
	}
}

func g(v any) {
	// ruleid: go-type-switch-stmt
	switch t := v.(type) {
	// ruleid: go-type-switch-case
	case a.b:
		_ = t
	}
}
