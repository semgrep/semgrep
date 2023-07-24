class BenchmarkTest02388 extends HttpServlet {
  @throws(classOf[ServletException])
  @throws(classOf[IOException])
  override def doPost(request: HttpServletRequest, response: HttpServletResponse): Unit = {
    val pWriter: PrintWriter = response.getWriter
    // ok: no-direct-response-writer
    pWriter.println("Hash Test java.security.MessageDigest.getInstance(java.lang.String) executed")
    // ruleid: no-direct-response-writer
    pWriter.println(request.input)

    val sWriter: SafeWriter = response.getSafeWriter
    // ok: no-direct-response-writer
    sWriter.println(request.input)
  }
}
