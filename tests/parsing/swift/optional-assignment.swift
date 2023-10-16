class Asdf {
    var x: Int? = nil
    var z: Int? = 0
}

var y = Asdf()

// Does not do the assignment, since y.x is nil
y.x? = 1
// Does the assignment, since y.z is 0
y.z? = 1

// prints None
print(y.x ?? "None")
// prints 1
print(y.z ?? "None")

var a: Int? = nil
var b: Int? = 0

a? = 1
b? = 1

print(a ?? "None")
print(b ?? "None")
