// ERROR:
cryptData.foo.bar.removeSubrange(numBytesEncrypted..<cryptData.count)
// ERROR:
cryptData.foo.removeSubrange(numBytesEncrypted..<cryptData.count)
// ERROR:
cryptData.removeSubrange(numBytesEncrypted..<cryptData.count)
// ERROR:
foo.removeSubrange(numBytesEncrypted..<cryptData.count)
// OK:
cryptData.removeSubrange(numBytesEncrypted..<cryptData.size)
// OK:
removeSubrange(numBytesEncrypted..<cryptData.count)
