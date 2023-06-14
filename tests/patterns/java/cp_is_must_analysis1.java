// https://github.com/returntocorp/semgrep/issues/7199
class A {
    
    public void method(boolean condition){
      var str = "hello";
      if(condition){
        if("goodbye".equals(str = "goodbye"))
            // OK: test
            X.test(str);
      }
      // OK: test
      X.test(str);
    }

    public void method2(boolean condition){
      var str = "hello";
      if(condition && "goodbye".equals(str = "goodbye")){
        // OK: test
        X.test(str);
      }
      // OK: test
      X.test(str);
    }

}
