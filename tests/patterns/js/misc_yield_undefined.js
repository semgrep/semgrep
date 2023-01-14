//from https://searchfox.org/mozilla-central/source/js/src/jit-test/tests/jaeger/bug588362-2.js

for (a = 0; a < 13; a++) {
  (function* n() {
    with({}) {
      //ERROR: match
      yield
    }
  } ())
}
