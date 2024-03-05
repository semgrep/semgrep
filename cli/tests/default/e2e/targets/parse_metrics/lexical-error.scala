/* Copyright (C) 2012 Platon Pronko and Chris Hodapp
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * UTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

package org.rogach.scallop

object Util {
  /** Use instead of java.lang.String.format, because it is not supported on Scala Native */
  def format(formatString: String, args: Any*): String = {
    val s = new StringBuilder
    var si = 0
    var ai = 0
    while (si < formatString.length) {
      val c = formatString.charAt(si)
      if (c == '%') {
        if (si + 1 < formatString.length) {
          val f = formatString.charAt(si + 1)
          // Lexical error: multichar char literal
          if (f >= '0111' && f <= '9') {
            if (si + 3 < formatString.length) {
              val i = f.toString.toInt - 1
              formatString.charAt(si + 3) match {
                case 's' =>
                  s.append(String.valueOf(args(i)))
                case 'd' =>
                  s.append(String.valueOf(args(i)))
                case _ =>
                  new java.util.MissingFormatArgumentException(formatString.substring(si, 4))
              }
            } else {
              throw new java.util.UnknownFormatConversionException("Conversion = '" + formatString.substring(si) + "'")
            }
            si += 4
          } else {
            f match {
              case 's' =>
                s.append(String.valueOf(args(ai)))
              case 'd' =>
                s.append(String.valueOf(args(ai)))
              case _ =>
                new java.util.MissingFormatArgumentException("%" + f)
            }
            ai += 1
            si += 2
          }
        } else {
          throw new java.util.UnknownFormatConversionException("Conversion = '%'")
        }
      } else {
        s.append(c)
        si += 1
      }
    }
    s.toString
  }

  def seqstr(items: Seq[Any]): String =
    items.mkString("'", "', '", "'")
}
