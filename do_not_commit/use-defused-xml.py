def bad():
    # ruleid: use-defused-xml
    import xml
    # ruleid: use-defused-xml
    from xml.etree import ElementTree 
    tree = ElementTree.parse('country_data.xml')
    root = tree.getroot()
    
def ok():
    # ok
    import defusedxml
    # ok
    from defusedxml.etree import ElementTree 
    tree = ElementTree.parse('country_data.xml')
    root = tree.getroot()
