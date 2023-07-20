class A {
    String str;
    public void method1(boolean condition){
        str = "hello";
        //ERROR: test
        X.test(str);
    }
    public void method2(boolean condition){
        if(condition){
            str = "hello";
        }
        //OK: test
        X.test(str);
    }
    public void method3(boolean condition){
        if(condition && str = "hello"){
        }
        //OK: test
        X.test(str);
    }
    public void method4(boolean condition){
        if(condition || str = "hello"){
        }
        //OK: test
        X.test(str);
    }
    public void method5(boolean condition){
        if(condition){
            str = "hello";
        }
        else {
            str = "hello";
        }
        //ERROR: test
        X.test(str);
    }
}
