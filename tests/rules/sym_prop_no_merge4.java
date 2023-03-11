package com.example.log4shell;

public class LoginServlet extends HttpServlet {
    @Override
    protected void doPost(HttpServletRequest req) {

        String userName = req.xyz;

        if(foo && x = 42) {
            //TODO: Since `x = 42` has side-effects, an `if (foo) x = 42` statement
            //      is added in the Dataflow IL before this if-statement. And because
            //      sym-prop values are killed at JOIN points, here we no longer have
            //      a sym-prop value for userName.
            //todoruleid: test
            baz(userName);
        }
    }
}
