trait Source extends geny.Writable{
  override def httpContentType = Some("application/octet-stream")
  def getHandle(): Either[geny.Writable, SeekableByteChannel]
  def writeBytesTo(out: java.io.OutputStream) = getHandle() match{
    case Left(bs) => bs.writeBytesTo(out)

    case Right(channel: FileChannel) =>
      val outChannel = Channels.newChannel(out)
      channel.transferTo(0, Long.MaxValue, outChannel)

    case Right(channel) =>
      val inChannel = Channels.newInputStream(channel)
      Internals.transfer(inChannel, out)
  }
}
