def a(): return 1
def b(): return
def c(): return None

class A:
    def a(self):
        return 1

class B:
    def b(self):
        #ruleid:
        return

class C:
    def c(self):
        #ruleid:
        return None

class D:
    def _d(self):
        return

class E:
    def _e(self):
        return None
