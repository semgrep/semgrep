# from https://github.com/returntocorp/semgrep/issues/2548
def a(): return 1
def b(): return
def c(): return None

class A:
    def a(self):
        return 1

class B:
    def b(self):
        #ruleid: should detect that
        return

class C:
    def c(self):
        #ruleid: should detect that
        return None

class D:
    def _d(self):
        return

class E:
    def _e(self):
        return None
