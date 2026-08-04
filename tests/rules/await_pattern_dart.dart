// Pattern `await fetch($URL)` previously misparsed at the file level as a
// bare `Call(fetch, [...])` with the `await` stripped, causing the rule to
// match plain `fetch(url)` calls (false positive). Statement-keyword
// routing should now produce an `Await(Call(...))` and match only awaited
// calls.

class C {
  Future<void> good() async {
    // ruleid: await-pattern-dart
    await fetch("https://example.com");
    // ruleid: await-pattern-dart
    await fetch("https://api.example.com/v1");
  }

  Future<void> bad() async {
    // ok: await-pattern-dart
    fetch("https://example.com");
    final pending = fetch("https://example.com");
    // ok: await-pattern-dart
    await pending;
  }
}
