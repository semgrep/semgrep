package com.example.log4shell;

public class LoginServlet extends HttpServlet {
    @Override
    protected void doPost(HttpServletRequest req) {

        String userName = req.xyz;

        if(foo && bar) {
            //ruleid: test
            baz(userName);
        }
        //ruleid: test
        baz(userName);
    }
}
