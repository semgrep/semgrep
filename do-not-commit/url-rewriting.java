package testcode.cookie;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public class UrlRewriting extends HttpServlet {

    @Override
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        encodeURLRewrite(resp, req.getRequestURI());
    }

    // ruleid: url-rewriting
    private String encodeURLRewrite(HttpServletResponse resp, String url) {
        return resp.encodeURL(url);
    }

    // ruleid: url-rewriting
    public String encodeUrlRewrite(HttpServletResponse resp, String url) {
        return resp.encodeUrl(url); //Deprecated
    }

    // ruleid: url-rewriting
    public String encodeRedirectURLRewrite(HttpServletResponse resp, String url) {
        return resp.encodeRedirectURL(url);
    }

    // ruleid: url-rewriting
    public String encodeRedirectUrlRewrite(HttpServletResponse resp, String url) {
        return resp.encodeRedirectUrl(url); //Deprecated
    }

    // ok
    public String encodeRedirectURLRewrite(SomeDifferentRequest resp, String url) {
        return resp.encodeURL(url);
    }

    // ok
    public String encodeRedirectUrlRewrite(HttpServletResponse resp, String url) {
        return resp.getHeader(url);
    }
}
