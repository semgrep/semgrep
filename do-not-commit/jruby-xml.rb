include ActiveSupport

 def bad_xml
     # ruleid: jruby-xml
     XmlMini.backend = 'JDOM'

     # ruleid: jruby-xml
     XmlMini.backend = 'LibXMLSAX'
 end

 def ok_xml
     # ok
     XmlMini.backend = 'REXML'
 end
