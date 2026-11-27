class T {
  void M(Shape s) {
    // ERROR:
    if (s is { Width: 0 }) { }
    // A different property name must not match.
    if (s is { Height: 0 }) { }
    // A different value must not match.
    if (s is { Width: 1 }) { }
    // Property patterns match exactly: an extra property, or a type before
    // the braces, is a different pattern.
    if (s is { Width: 0, Height: 1 }) { }
    if (s is Rect { Width: 0 }) { }
  }
}
