void main() {
  // ruleid: metavar-regex-dart
  var insecure_token = "abc";
  // ruleid: metavar-regex-dart
  var insecure_path = "/tmp";
  // ok: metavar-regex-dart
  var secure_token = "abc";
  // ok: metavar-regex-dart
  var path = "/tmp";
}
