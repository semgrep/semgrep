local all = import "p/ocaml";

{ rules:
    [if r.languages == ["ocaml"]
      then r + { paths: { exclude: ["test.ml"]} }
      else r
     for r in all.rules
    ]
}
