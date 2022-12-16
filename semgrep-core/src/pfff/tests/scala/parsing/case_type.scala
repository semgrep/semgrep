/**
  * Creates a symbolic link between two paths
  */
object symlink {
  def apply(link: Path, dest: FilePath, perms: PermSet = null): Unit = {
    val permArray: Array[FileAttribute[_]] =
      if (perms == null) Array[FileAttribute[_]]()
      else Array(PosixFilePermissions.asFileAttribute(perms.toSet()))

    Files.createSymbolicLink(
      link.toNIO,
      dest match{
        // Special case empty relative paths, because for some reason `createSymbolicLink`
        // doesn't like it when the path is "" (most other Files.* functions are fine)
        case p: RelPath if p.segments.isEmpty && p.ups == 0 => java.nio.file.Paths.get(".")
        case p: SubPath if p.segments.isEmpty => java.nio.file.Paths.get(".")
        case _ => dest.toNIO
      },
      permArray:_*
    )
  }
}
