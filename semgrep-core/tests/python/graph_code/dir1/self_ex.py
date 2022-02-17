class Ex:
    fld1: int
    fld2_dead_ok: int

    def bar():
        return 1

    def other_dead_ok():
        return 1
    
    def foo():
        x = self.fld1
        self.bar(x)
        return 0

    
def test_dead_ok():
    Ex().foo();
