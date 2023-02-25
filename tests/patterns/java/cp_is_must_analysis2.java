// https://github.com/returntocorp/semgrep/issues/7199
class A {
    
    public void method(boolean condition){
      var str = "hello";
      if(condition){
        if("goodbye".equals(str = "goodbye"))
            // ERROR: test
            X.test(str);
      }
      // OK: test
      X.test(str);
    }

    public void method2(boolean condition){
      var str = "hello";
      /*
      In the IL this will be viewed as:

      if (condition)
        str = "goodbye";
      if(condition && "goodbye".equals(str)) {
        X.test(str);
      }

      And const-prop is a path-insensitive analysis.
      */
      if(condition && "goodbye".equals(str = "goodbye")){
        // TODO: test
        X.test(str);
      }
      // OK: test
      X.test(str);
    }

}
