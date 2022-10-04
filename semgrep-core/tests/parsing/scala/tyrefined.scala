package com.abc.extdata.core.util

import java.io.File

import scala.concurrent.{ExecutionContext, Future}
import scala.language.reflectiveCalls

/**
  * @author pavkir
  */
trait IOUtil {

  def using[C <: { def close(): Unit }, R](obj: C)(action: C => R): R = {
    try action(obj)
    finally if (obj != null) obj.close()
  }

  def usingTmpFile[A](prefix: String)(f: File => A): A = {
    val file = File.createTempFile(prefix, ".tmp")
    file.deleteOnExit()
    val w = f(file)
    file.delete()
    w
  }
}

object IOUtil extends IOUtil
