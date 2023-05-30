// https://github.com/returntocorp/semgrep/issues/7694
// We don't want to get: "Fatal error": (Failure "Call AST_utils.with_xxx_equal to avoid this error.")
$('.invalid', c).foobar(
  function() {
      //ruleid: test
      $l = $t.baz('<foo for="' + id + '"></foo>');
  }
);
