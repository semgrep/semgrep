{ rules: [
// comments are ok (C++ or Shell syntax)
{
  # no need for “id”:
  id: "my-rule",
  # multilines strings
  pattern: |||
      foo($X)
      ... 
      bar($X)
   |||,
  message: "you should not do that",
  languages: ["python"],
  severity: "ERROR", # trailing commas are ok
}
]}
