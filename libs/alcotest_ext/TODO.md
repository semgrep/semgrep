To-do list for Alcotest_ext
==

This list should be converted into GitHub issues once Alcotest_ext has
its own repository.

Things to do:

* rename Alcotest_ext to something shorter
* make color output optional (on/off/auto)
* protect against tests that might change terminal settings mid-run
  and cause color to disappear mid-run (already an issue with plain Alcotest)
* investigate whether an Lwt- or Async- specific interface is necessary.
  If not, drop the Lwt-specific interface.
* make the output more readable
