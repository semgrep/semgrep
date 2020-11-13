 def bad_escape
     # ruleid: json-entity-escape
     ActiveSupport.escape_html_entities_in_json = false
 end

 def ok_escape
     # ok
     ActiveSupport.escape_html_entities_in_json = true
 end
