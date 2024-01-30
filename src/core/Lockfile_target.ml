type t = {
  lockfile : Fpath.t;
  ecosystem : Supply_chain.ecosystem;
  lazy_lockfile_content : string lazy_t;
  lazy_lockfile_ast_and_errors : Supply_chain.dependency list lazy_t;
}
