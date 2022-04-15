import javax.servlet.*;
  
public class Demo {

   protected void service(HttpServletRequest request, HttpServletResponse response)
                    throws ServletException, IOException {

        //ERROR: match
        String queryString = request.getQueryString();

        //ERROR: match
        String queryString = (new HttpServletRequest()).getQueryString();

        //not this one
        String myQuery = mc.getQueryString();

        System.out.println("Request: "+reqUrl + "?" + queryString);
    }
}
