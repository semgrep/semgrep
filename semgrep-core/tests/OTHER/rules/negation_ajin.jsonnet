local s = import 'lib_semgrep.jsonnet';

s.basic_rule('python', 
             s.And('os.environ',
                   s.Not(s.Or('os.environ.get(...)',
                              'os.environ[...]'))))
