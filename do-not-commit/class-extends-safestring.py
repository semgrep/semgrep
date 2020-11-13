from django.utils.safestring import SafeData
from django.utils.safestring import SafeString
from django.utils.safestring import SafeText

# ruleid:class-extends-safestring
class IWantToBypassEscaping(SafeString):
    def __init__(self):
        super().__init__()


# ruleid:class-extends-safestring
class IWantToBypassEscaping2(SafeText):
    def __init__(self):
        super().__init__()


# ruleid:class-extends-safestring
class IWantToBypassEscaping3(SafeData):
    def __init__(self):
        super().__init__()


# ok:class-extends-safestring
class SomethingElse(str):
    def __init__(self):
        super().__init__()
