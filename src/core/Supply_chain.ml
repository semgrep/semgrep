type dependency = {
  package_name : string;
  package_version : string;
  ecosystem : ecosystem;
  transitivity : transitivity;
  url : string option;
  loc : Tok.location * Tok.location;
  toks : Tok.t list;
}

and ecosystem = Npm
and transitivity = Direct | Transitive | Unknown [@@deriving show, eq]
