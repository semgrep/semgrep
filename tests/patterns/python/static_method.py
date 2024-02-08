class A:
    #MATCH:
    @staticmethod
    def f():
        pass

    #MATCH:
    @staticmethod()
    def f():
        pass

    #OK:
    @staticmethod("A ERROR")
    def f():
        pass
