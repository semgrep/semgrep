package Foo;

//ERROR: match
func (o *Type) Method(a *Args,
         y *Reply) error {
    longcode()
    longcode()
    longcode()
    longcode()
    longcode()
    return 1
}
