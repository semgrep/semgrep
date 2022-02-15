from dir1.a import A

class B(A):
    def bar():
        return 3
    

def use_A_dead_ok():
    return A().foo()
    
