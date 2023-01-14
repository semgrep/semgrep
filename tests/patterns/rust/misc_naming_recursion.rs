// this was causing -dump_named_ast to recurse indefinitely
// because it was parsed as fn accumulate(self: self) and
// Naming_AST will add "self", Ty self in the environment, and
// when we processed the self type, we would look for it in the env
// and find "self" --> Ty Self and set id_type to itself, creating
// a cycle in the AST.

fn accumulate(self) {}
