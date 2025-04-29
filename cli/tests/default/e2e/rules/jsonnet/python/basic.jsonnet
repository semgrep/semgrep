local example = import '../example_rule.yaml';
local partial = import '../part_of_rule.unknown_yaml_extension';
local lib = import 'template.libsonnet';
{
  rules: [lib {
    ruleid: 'basic',
    message: example.rules[0].message,
    languages: partial.languages,
    pattern: '$X == $X',
  }],
}
