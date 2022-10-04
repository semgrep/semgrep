public class BenchmarkTest02388 extends HttpServlet {

    public void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        // OK because constant string
        // ok: no-direct-response-writer
        PrintWriter writer = response.getWriter();
        writer.println(
"Hash Test java.security.MessageDigest.getInstance(java.lang.String) executed"
);
    }  // end doPost

}
