open Spacegrep

let () =
  Parse.of_stdin ()
  |> Print.to_stdout

