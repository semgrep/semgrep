class A:
    #MATCH:
    @staticmethod
    def f0():
        pass

    # Not dynamically correct python, but just showing how it behaves
    # for pattern matching.

    #MATCH:
    @staticmethod()
    def f1():
        pass

    # Not dynamically correct python, but just showing how it behaves
    #for pattern matching.

    # OK:
    @staticmethod("A ERROR")
    def f2():
        pass
