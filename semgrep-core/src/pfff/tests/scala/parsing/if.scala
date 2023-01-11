package os

import java.nio.file.{Files, LinkOption}
import java.nio.file.attribute.{GroupPrincipal, PosixFileAttributeView, UserPrincipal}

/**
  * Get the filesystem permissions of the file/folder at the given path
  */
object perms extends Function1[Path, PermSet]{
  def apply(p: Path): PermSet = apply(p, followLinks = true)
  def apply(p: Path, followLinks: Boolean = true): PermSet = {
    val opts = if (followLinks) Array[LinkOption]() else Array(LinkOption.NOFOLLOW_LINKS)
    PermSet.fromSet(Files.getPosixFilePermissions(p.wrapped, opts:_*))
  }
  /**
    * Set the filesystem permissions of the file/folder at the given path
    *
    * Note that if you want to create a file or folder with a given set of
    * permissions, you can pass in an [[os.PermSet]] to [[os.write]]
    * or [[os.makeDir]]. That will ensure the file or folder is created
    * atomically with the given permissions, rather than being created with the
    * default set of permissions and having `os.perms.set` over-write them later
    */
  object set {
    def apply(p: Path, arg2: PermSet): Unit = {
      Files.setPosixFilePermissions(p.wrapped, arg2.toSet())
    }
  }

}
