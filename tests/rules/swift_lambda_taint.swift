       
let foo = source 

let bar = qux { x in
    // ruleid: swift-lambda-taint
    sink(foo)
}