local s = import 'lib_semgrep.jsonnet';

s.basic_rule('python', 
             s.And('foo($ARG)',
                   s.Where('semgrep_re_match($ARG, ".*bar.*")')))

