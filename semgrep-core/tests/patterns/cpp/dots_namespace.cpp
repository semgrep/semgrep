namespace A {
}

//ERROR: match
namespace VM {
int foo() { }
}

namespace A {
}

namespace B {
  //ERROR: match
  namespace VM {
    
  }
}
