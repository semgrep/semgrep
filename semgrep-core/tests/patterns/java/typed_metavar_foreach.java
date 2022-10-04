public class OtherObj {
  void stateChange(Iterable<MyObj> objs, String arg0, String arg1) {
    for (MyObj obj : objs) {
      //ERROR:match
      obj.persistObj(arg0, arg1);
    }

    MyObj obj = new MyObj();
    //ERROR:match   
    obj.persistObj(arg0, arg1);
  }
}
