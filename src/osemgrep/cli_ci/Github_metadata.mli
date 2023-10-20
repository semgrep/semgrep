type env = {
  git : Git_metadata.env;
  _GITHUB_EVENT_JSON : Yojson.Basic.t;
  _GITHUB_REPOSITORY : string option;
  _GITHUB_API_URL : Uri.t option;
  _GITHUB_SHA : Digestif.SHA1.t option;
  _GITHUB_SERVER_URL : Uri.t;
  _GITHUB_REF : string option;
  _GITHUB_HEAD_REF : string option;
  _GITHUB_RUN_ID : string option;
  _GITHUB_EVENT_NAME : string option;
  _GH_TOKEN : string option;
}

include Project_metadata.S with type env := env
(** Gather metadata from GitHub Actions. *)
