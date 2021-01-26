{ rules: [
    # Basic rules
    {
      id: "stupid_equal",
      match: "$X == $X",
      message: "Dude, $X == $X is always true (Unless X is NAN ...)",
      languages: [ "python", "javascript", "ruby", "c", "go", "ocaml" ],
      severity: "WARNING",
    },
    {
      id: "stupid_not_equal",
      match: "$X != $X",
      message: "Dude, $X != $X is always false (Unless X is NAN ...)",
      languages: [ "python", "javascript" ],
      severity: "WARNING"
    },
    {
      id: "stupid_conditional",
      match: |||
        if $E:
          $S
        else:
          $S
      |||,
      message: "Dune, both branches are equal",
      languages: [  "python" ],
      severity: "WARNING"
    }
  ]
}
