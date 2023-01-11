case class CommandResult(exitCode: Int) {
  val (out, err) = {
    chunks.foreach{
      case Left(s) => outChunks.append(s)
      case Right(s) => errChunks.append(s)
    }
    (geny.ByteData.Chunks(outChunks.toSeq), geny.ByteData.Chunks(errChunks.toSeq))
  }
}
