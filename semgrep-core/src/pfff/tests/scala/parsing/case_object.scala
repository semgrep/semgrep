sealed trait FileType
object FileType{
  case object File extends FileType
  case object Dir extends FileType
  case object SymLink extends FileType
  case object Other extends FileType
}
