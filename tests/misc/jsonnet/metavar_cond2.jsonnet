local s = import 'lib_semgrep.jsonnet';

s.basic_rule('python', 
             s.And('foo($ARG)',
                   s.Where('$ARG > 10')))

