package com.example.log4shell;

public class LoginServlet extends HttpServlet {
    @Override
    protected void doPost(HttpServletRequest req) {

        String userName = req.xyz;

        if(foo && bar) {
            //ruleid: test
            baz(userName);
        }
        // TODO: Right now we kill all sym-prop values at JOIN points (i.e. after if-
        //       or loop statements). In principle we would not have to do this when
        //       all the branches arriving at the JOIN carry the same sym-prop value.
        //todoruleid: test
        baz(userName);
    }
}
