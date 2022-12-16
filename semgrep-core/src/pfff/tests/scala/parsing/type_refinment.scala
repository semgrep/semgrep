trait ReflectionToolkit {
  /*
  val global: Global
  import global._
  lazy val g: global.type = global

  lazy val isDocCompiler = global.isInstanceOf[ScaladocGlobal]
  lazy val isReplCompiler = global.isInstanceOf[ReplGlobal]
  lazy val isInteractiveCompiler = global.isInstanceOf[InteractiveGlobal]
  // NOTE: InteractiveGlobal does not work with semanticdb-scalac, in scalameta/language-server
  // we tried to enable semanticdb-scalac with the presentation compiler and it resulted
  // in cryptic infinite while loops while completing scope members.
  lazy val isSupportedCompiler = !isDocCompiler && !isReplCompiler && !isInteractiveCompiler
  */
  // NOTE: this boilerplate is unfortunately necessary, because we don't expose Attachable in the public API
  trait Attachable[-T] {
    def attachments(carrier: T): Attachments { type Pos = Position }
    def updateAttachment[U: ClassTag](carrier: T, attachment: U): Unit
  }
}
