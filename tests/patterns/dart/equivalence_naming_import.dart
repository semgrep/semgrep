import 'package:http/http.dart' as h;
import 'dart:async' as async;
import 'package:http/http.dart' as alt;

void main() {
  // ERROR:
  h.get('https://example.com');

  // ERROR:
  alt.get('https://example.com');

  // OK:
  async.run(() => null);

  // OK:
  h.post('https://example.com');
}
