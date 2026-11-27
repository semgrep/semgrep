class C {
  void M() {
    // ERROR:
    try { f(); }
    catch (IOException e) when (Log(e)) { }

    // No filter: no match.
    try { f(); }
    catch (IOException e) { }

    // Another filter: no match.
    try { f(); }
    catch (IOException e) when (e.HResult == 5) { }
  }
}
