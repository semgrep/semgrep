@attr.s(auto_attribs=True)
class ExampleClass(object):
    # ruleid:attr-mutable-initializer
    empty_dict = {}
    # ruleid:attr-mutable-initializer
    empty_list = []
    # ruleid:attr-mutable-initializer
    somedict = dict()
    # ruleid:attr-mutable-initializer
    somelist = list()
    # ruleid:attr-mutable-initializer
    someset = set()
    # ok
    ex_good1 = attr.ib(factory=dict)
    # ok
    ex_good2 = attr.ib(factory=list)
    # ok
    ex_good3 = attr.ib(factory=set)
    # ruleid:attr-mutable-initializer
    myset = {1, 2, 3}
    
    def foo(self):
        # ok
        x = {}
        
    def bar(self) -> int:
        #ok
        thisset = set()