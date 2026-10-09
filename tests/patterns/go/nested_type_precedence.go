package precedence

// ERROR: match
var pointer *func() *int
var function func() *int
var otherPointer *func() int
