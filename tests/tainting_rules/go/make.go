// simple taint
func simpleTaint() int {
    global := getGlobal()
    items := make(global.Items, len(global.items))
    //ruleid: taint-example
    sink(global)
}

// simple sanitization is ok
func simpleSanitize() int {
    global := getGlobal()
    items := make(global.Items, len(global.items))
    sanitizedGlobal := sanitizeGlobal(global)
    //OK:
    sink(sanitizedGlobal)
}

// 2 hop taint
func twoHopTaint() int {
    global := getGlobal()
    items := make(global.Items, len(global.items))
    //ruleid: taint-example
    sink(items)
}

// 2 hop + sanitization is ok
func twoHopTaintSanitized() int {
    global := getGlobal()
    items := make(global.Items, len(global.items))
    sanitizeGlobal(items)
    //TODO
    //ruleid: taint-example
    sink(items)
}

// Conditional taint
func GetItems() int {
    global := getGlobal()
    items := make(global.Items, len(global.items))
    if len(items) == 0 {
        fmt.Println("No items")
        //ruleid: taint-example
        sink(global)
    }
    return 0
}
