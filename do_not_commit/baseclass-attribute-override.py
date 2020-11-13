
# ruleid: baseclass-attribute-override
class A:
    def method1(self, args):
        pass


class A2:
    def method2(self, args):
        pass


class B:
    def method1(self, args):
        print('hello there')

class C(A, B):
    def __init__():
        print('initialized')
