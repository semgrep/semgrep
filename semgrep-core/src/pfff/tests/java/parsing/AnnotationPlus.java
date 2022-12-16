class Foo {
@Option(
      name = ADB_THREADS_LONG_ARG,
      aliases = {ADB_THREADS_SHORT_ARG},
      usage =
          "Number of threads to use for adb operations. "
               + "Defaults to number of connected devices.")
  private int adbThreadCount = 0;
}
