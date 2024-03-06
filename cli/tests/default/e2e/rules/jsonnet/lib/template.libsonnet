{
  local rule = self,
  ruleid:: error 'Must override the rule id',
  local severity = "ERROR",

  id: rule.ruleid,
  severity: severity,
}
