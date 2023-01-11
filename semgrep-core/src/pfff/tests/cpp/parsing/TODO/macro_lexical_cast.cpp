int main(int argc, char** argv) {
  initFacebook(&argc, &argv);

  // If host and port is not specified, start a local memcache
  int port = 0;
  if (FLAGS_host.empty() && FLAGS_port.empty()) {
    cout << "\nHost not specified, starting local memcached..." << endl;
    FLAGS_host = "localhost";
    port = MemcacheTestUtil::findFreeListenPort();
    MemcacheTestUtil::startMemcached(port);
    FLAGS_port = lexical_cast<string>(port);
  }
}
