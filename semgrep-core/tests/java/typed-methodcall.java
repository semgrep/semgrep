import javax.servlet.*;
  
public class Demo {

   protected void service(HttpServletRequest request, HttpServletResponse response)
                    throws ServletException, IOException {

        MyCustomClass mc = new MyCustomClass();

        request.setCharacterEncoding("UTF-8");

        String reqUrl = request.getRequestURL().toString();
        //ERROR: match
        String queryString = request.getQueryString();

        //not this one
        String myQuery = mc.getQueryString();

        System.out.println("Request: "+reqUrl + "?" + queryString);
    }
}
