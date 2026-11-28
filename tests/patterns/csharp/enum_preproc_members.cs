// Members inside #if usually carry their own commas.
// ERROR:
enum E {
  A,
#if X
  Secret,
#elif Y
  Other,
#else
  Third,
#endif
  B,
}

enum F {
  A,
#if X
  Secret,
#endif
}

// The upstream form: the comma after `#endif`.
// ERROR:
enum G {
  A,
#if X
  Other
#endif
  , B
}

// The comma at the start of the `#if` content.
// ERROR:
enum H {
  A
#if X
  , Other
#endif
}
