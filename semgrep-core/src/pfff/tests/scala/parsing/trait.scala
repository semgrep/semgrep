package os

import collection.JavaConverters._

trait PathChunk{
  def segments: Seq[String]
  def ups: Int
}
