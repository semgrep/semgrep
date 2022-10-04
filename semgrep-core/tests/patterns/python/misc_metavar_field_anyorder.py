#from AModule.Part import Vuln
#from ThisModule import SuperClass

# this used to not match because Assign in Python were not
# translated in DefStmt, which is the only thing that was matched
# in any order in Generic_vs_generic.m_fields.

#ERROR: match
class LOL(SuperClass):
    hah = 1
    data = Vuln()
    
