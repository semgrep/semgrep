
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let verbose = false

let tests =
  Testutil.pack_suites "analyze_ml" [

    (*****************************************************************************)
    (* Database building *)
    (*****************************************************************************)
    [
      "building light database", (fun () ->
        let data_dir = Config_pfff.tests_path "ml/db" in
        let _db = Database_light_ml.compute_database ~verbose [data_dir] in
        ()
      )
    ];

    (*****************************************************************************)
    (* Coverage *)
    (*****************************************************************************)
    Testutil.pack_tests "coverage_ml" ([

      "basename to readable", (fun () ->
        let dummy_coverage = { Coverage_code.
                               covered_sites = []; all_sites = []
                             }
        in
        let cover = [
          ("unit_analyze_ml.ml", dummy_coverage);
          ("coverage_ml.ml", dummy_coverage);
        ]
        in
        let root = Filename.concat Config_pfff.path_pfff_home "lang_ml" in
        let cover' =
          Coverage_ml.basename_coverage_to_readable_coverage cover root
          |> List.map fst
        in
        Alcotest.(check (list string))
          "it should map basename'd files to readable paths"
          ["analyze/unit_analyze_ml.ml";
           "analyze/coverage_ml.ml";
          ]
          cover'
      );
    ]);


    (*****************************************************************************)
    (* Postlude *)
    (*****************************************************************************)
  ]
