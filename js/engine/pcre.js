//Provides: pcre_ocaml_init const
function pcre_ocaml_init() {
  // noop
}

//Provides: pcre_version_stub const
function pcre_version_stub() {
  return "8.45 Javascript";
}

//Provides: pcre_config_utf8_stub const
function pcre_config_utf8_stub() {
  return 1;
}

//Provides: pcre_config_newline_stub
function pcre_config_newline_stub() {
  return 0; // TODO: correct?
}

//Provides: pcre_config_link_size_stub_bc
function pcre_config_link_size_stub_bc() {
  return 2;
}

//Provides: pcre_config_match_limit_stub_bc
function pcre_config_match_limit_stub_bc() {
  return 10000000;
}

//Provides: pcre_config_match_limit_recursion_stub_bc
function pcre_config_match_limit_recursion_stub_bc() {
  return 10000000;
}

//Provides: pcre_config_stackrecurse_stub
function pcre_config_stackrecurse_stub() {
  return 1;
}

//Provides: pcre_compile_stub_bc
function pcre_compile_stub_bc(v_opt, v_tables, v_pat) {
  return new RegExp(v_pat); // TODO: handle options
}

//Provides: pcre_study_stub
function pcre_study_stub() {
  // noop
}

//Provides: pcre_exec_stub_bc
function pcre_exec_stub_bc(
  v_opt,
  v_rex,
  v_pos,
  v_subj_start,
  v_subj,
  v_ovec,
  v_maybe_cof,
  v_workspace
) {
  if (v_subj_start != 0) {
    throw new Error(`v_subj_start = ${v_subj_start}`);
  }
  if (v_maybe_cof) {
    throw new Error("callout functions unimplemented");
  }

  v_rex.exec(v_subj);
}

//Provides: pcre_set_imp_match_limit_stub_bc
function pcre_set_imp_match_limit_stub_bc(rex) {
  return rex;
}

//Provides: pcre_set_imp_match_limit_recursion_stub_bc
function pcre_set_imp_match_limit_recursion_stub_bc(rex) {
  return rex;
}

//Provides: pcre_capturecount_stub_bc
function pcre_capturecount_stub_bc(rex) {
  return 0; // TODO
}

//Provides: octs_create_parser_jsonnet
function octs_create_parser_jsonnet() {
  return null;
}

//Provides: octs_create_parser_typescript
function octs_create_parser_typescript() {
  return null;
}

//Provides: octs_create_parser_tsx
function octs_create_parser_tsx() {
  return null;
}
