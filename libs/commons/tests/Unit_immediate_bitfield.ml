open Immediate_bitfield

let test_roundtrip () =
  assert (not (get_bit empty 0));
  assert (not (get_bit (set_bit empty 0 false) 0));
  assert (get_bit (set_bit empty 0 true) 0);
  assert (not (get_bit (set_bit (set_bit empty 0 true) 0 false) 0));
  assert (not (get_bit (set_bit (set_bit empty 1 true) 0 false) 0));
  assert (not (get_bit (set_bit (set_bit empty 1 true) 1 true) 0));
  assert (get_bit (set_bit (set_bit empty 1 true) 0 false) 1);
  ()

let tests =
  Testutil.pack_suites "common"
    [
      Testutil.pack_suites "immediate bitfield"
        [ [ ("roundtrip", test_roundtrip) ] ];
    ]
