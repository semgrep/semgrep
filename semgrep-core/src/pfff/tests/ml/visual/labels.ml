
let bar int string =
  ()


let foo ~label1 str =
  ()


let cst = 1

let test () =
  foo ~label1:1 "foo";
  foo ~cst "foo"
