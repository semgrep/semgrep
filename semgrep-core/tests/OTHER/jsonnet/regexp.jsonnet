local s = import 'lib_semgrep.jsonnet';

s.basic_rule('python', 
             s.And('foo("...")',
                   s.Regex(@'\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}')))


