case class PermSet(value: Int) {
  def contains(elem: PosixFilePermission) = (PermSet.permToMask(elem) & value) != 0
  def +(elem: PosixFilePermission) = new PermSet(value | PermSet.permToMask(elem))
  def ++(other: PermSet) = new PermSet(value | other.value)

  def -(elem: PosixFilePermission) = new PermSet(value & (~PermSet.permToMask(elem)))
  def --(other: PermSet) = new PermSet(value & (~other.value))

  def toInt(): Int = value
}
