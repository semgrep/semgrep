{ rules: [
  { id: "my-rule",
    match:
     # no and-inside: or pattern-inside:
     { and: [|||
          def foo(...):
            ... 
        |||,
        { or: [
          "foo($X, $S)", 
          "bar($X, $S)",
         ],
        # no metavar-comparison:, or metavar-regex:, or metavar: and strip: and ...
        where: '$X > 42 && re.match($S,"a.*")',
       }]
     },
    message: "you should not do that",
    languages: ["python"],
    severity: "ERROR",
  }
 ]
}
