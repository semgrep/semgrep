local lib = import 'template.libsonnet';
local example = import '../example_rule.yaml';
local partial = import '../part_of_rule.unknown_yaml_extension';
{
  'rules': [ lib {
    ruleid: 'basic',
    'message': example.rules[0].message,
    'languages': partial.languages,
    'pattern': '$X == $X'
  } ]
}
