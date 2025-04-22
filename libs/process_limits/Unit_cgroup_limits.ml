(*
   Unit tests for reading the files containing cgroup limits.

   These files are emulated so that we don't have to run a special Linux
   kernel and/or container.

   For real testing, you would need a Linux kernel that runs cgroups v1 or v2.
   v1 can be turned on at boot time with a kernel that supports v2.
   Setting CPU limits can then be done by running a Docker container
   with e.g. 'docker run -it --cpus=3.5 ...'.
*)

open Common

type cgroup_data = {
  cpu_quota_v1 : string option;
  cpu_period_v1 : string option;
  cpu_quota_period_v2 : string option;
}

(* Map optional file content to an optional temporary file path
   and run func in that environment. *)
let with_opt_temp_file opt_contents func =
  match opt_contents with
  | None -> func None
  | Some contents ->
      UTmp.with_temp_file ~contents (fun path -> func (Some path))

let with_temp_cgroup_files cgroup_data func =
  with_opt_temp_file cgroup_data.cpu_quota_v1 (fun cpu_quota_path_v1 ->
      with_opt_temp_file cgroup_data.cpu_period_v1 (fun cpu_period_path_v1 ->
          with_opt_temp_file cgroup_data.cpu_quota_period_v2
            (fun cpu_quota_period_path_v2 ->
              let cgroup_files : Cgroup_limits.cgroup_files =
                {
                  cpu_quota_path_v1;
                  cpu_period_path_v1;
                  cpu_quota_period_path_v2;
                }
              in
              func cgroup_files)))

let default =
  { cpu_quota_v1 = None; cpu_period_v1 = None; cpu_quota_period_v2 = None }

type test_spec = {
  name : string;
  input : cgroup_data;
  expected_output : (Cgroup_limits.max_cpus, unit) result;
}

let test_data : test_spec list =
  let open Cgroup_limits in
  [
    (* Common, expected situations *)
    { name = "no cgroups"; input = default; expected_output = Error () };
    {
      name = "cgroups v1 limit";
      input =
        {
          default with
          cpu_quota_v1 = Some "250000\n";
          cpu_period_v1 = Some "100000\n";
        };
      expected_output = Ok (CPU_limit 2.5);
    };
    {
      name = "cgroups v2 limit";
      input = { default with cpu_quota_period_v2 = Some "250000 100000\n" };
      expected_output = Ok (CPU_limit 2.5);
    };
    {
      name = "cgroups v1 no limit";
      input =
        {
          default with
          cpu_quota_v1 = Some "-1\n";
          cpu_period_v1 = Some "100000\n";
        };
      expected_output = Ok No_CPU_limit;
    };
    {
      name = "cgroups v2 no limit";
      input = { default with cpu_quota_period_v2 = Some "max 100000\n" };
      expected_output = Ok No_CPU_limit;
    };
    (* Dysfunctional situations *)
    {
      name = "cgroups v1 broken";
      input = { default with cpu_quota_v1 = Some ""; cpu_period_v1 = Some "" };
      expected_output = Error ();
    };
    {
      name = "cgroups v2 broken";
      input = { default with cpu_quota_period_v2 = Some "" };
      expected_output = Error ();
    };
    {
      name = "cgroups v2 negative value";
      input = { default with cpu_quota_period_v2 = Some "-1 1\n" };
      expected_output = Error ();
    };
  ]

let testable : (Cgroup_limits.max_cpus, unit) result Alcotest.testable =
  let print fmt (x : (Cgroup_limits.max_cpus, unit) result) =
    Format.fprintf fmt "%s"
      (match x with
      | Ok No_CPU_limit -> "No_CPU_limit"
      | Error () -> "error"
      | Ok (CPU_limit x) -> spf "CPU_limit %g" x)
  in
  Alcotest.testable print ( =*= )

let make_test (x : test_spec) =
  Testo.create ~category:[ "cgroup limits"; "CPU limits" ] x.name (fun () ->
      with_temp_cgroup_files x.input (fun cgroup_files ->
          let res = Cgroup_limits.get_max_cpus ~cgroup_files () in
          Alcotest.check testable "CPU limit" x.expected_output res))

let tests : Testo.t list = List_.map make_test test_data
