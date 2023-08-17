class BenchmarkTest02388 : HttpServlet() {
    @Throws(ServletException::class, IOException::class)
    override fun doPost(request: HttpServletRequest, response: HttpServletResponse) {
        val pWriter: PrintWriter = response.getWriter()
        // ok: no-direct-response-writer
        pWriter.println("Hash Test java.security.MessageDigest.getInstance(java.lang.String) executed")
        // ruleid: no-direct-response-writer
        pWriter.println(request.input)

        val sWriter: SafeWriter = response.getSafeWriter()
        // ok: no-direct-response-writer
        sWriter.println(request.input)
    }
}
