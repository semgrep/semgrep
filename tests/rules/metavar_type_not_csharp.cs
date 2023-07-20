public class BenchmarkTest02388 : IHttpHandler
{
    public void ProcessRequest(HttpContext context)
    {
        HttpResponse response = context.Response;
        HttpRequest request = context.Request;

        // ok: no-direct-response-writer
        response.Write("Hash Test java.security.MessageDigest.getInstance(java.lang.String) executed");
        // ruleid: no-direct-response-writer
        response.Write(request.Form["input"]);

        // Assuming response has a getSafeWriter() method
        SafeWriter sWriter = response.getSafeWriter();
        // ok: no-direct-response-writer
        sWriter.Write(request.Form["input"]);
    }

    public bool IsReusable => false;
}
