package os

package object watch{
  /**
    * Efficiently watches the given `roots` folders for changes. Any time the
    * filesystem is modified within those folders, the `onEvent` callback is
    * called with the paths to the changed files or folders.
    *
    * Once the call to `watch` returns, `onEvent` is guaranteed to receive a
    * an event containing the path for:
    *
    * - Every file or folder that gets created, deleted, updated or moved
    *   within the watched folders
    *
    * - For copied or moved folders, the path of the new folder as well as
    *   every file or folder within it.
    *
    *
    * - For deleted or moved folders, the root folder which was deleted/moved,
    *   but *without* the paths of every file that was within it at the
    *   original location
    *
    * Note that `watch` does not provide any additional information about the
    * changes happening within the watched roots folder, apart from the path
    * at which the change happened. It is up to the `onEvent` handler to query
    * the filesystem and figure out what happened, and what it wants to do.
    *
    * `watch` currently only supports Linux and Mac-OSX, and not Windows.
    */
  def watch(roots: Seq[os.Path],
            onEvent: Set[os.Path] => Unit,
            logger: (String, Any) => Unit = (_, _) => ()): AutoCloseable  = {
    val watcher = System.getProperty("os.name") match{
      case "Linux" => new os.watch.WatchServiceWatcher(roots, onEvent, logger)
      case "Mac OS X" => new os.watch.FSEventsWatcher(roots, onEvent, logger, 0.05)
      case osName => throw new Exception(s"watch not supported on operating system: $osName")
    }

    val thread = new Thread {
      override def run(): Unit = {
        watcher.run()
      }
    }
    thread.setDaemon(true)
    thread.start()
    watcher
  }
}
