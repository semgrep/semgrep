package testcode;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public class UnvalidatedRedirectServlet extends HttpServlet {

    @Override
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        String url = req.getParameter("urlRedirect");
        unvalidatedRedirect1(resp, url);
    }

    // ruleid: unvalidated-redirect
    private void unvalidatedRedirect1(HttpServletResponse resp, String url) throws IOException {
        if (url != null) {
            resp.sendRedirect(url);
        }
    }

    // ruleid: unvalidated-redirect
    public void unvalidatedRedirect2(HttpServletResponse resp, String url) {
        if (url != null) {
            resp.addHeader("Location", url);
        }
    }

    // ruleid: unvalidated-redirect
    private void unvalidatedRedirect3(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        resp.sendRedirect(req.getParameter("urlRedirect"));
    }

    // ruleid: unvalidated-redirect
    public void unvalidatedRedirect4(HttpServletRequest req, HttpServletResponse resp) {
        String url = req.getParameter("urlRedirect");
        resp.addHeader("Location", url);
    }

    // ok
    public void falsePositiveRedirect1(HttpServletResponse resp) throws IOException {
        String url = "/Home";
        if (url != null) {
            resp.sendRedirect(url);
        }
    }

    // ok
    public void falsePositiveRedirect2(HttpServletResponse resp) {
        resp.addHeader("Location", "/login.jsp");
    }
}
