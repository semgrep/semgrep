import org.apache.log4j.LogManager;

import java.io.*;
import java.sql.SQLException;
import java.util.*;


public class MyJavaClass {
    //ERROR: match
    MyJavaClass() {
        // Constructor
    }

    public static final Logger logger = LoggerFactory.getLogger(someClass.class);;
    public static void someFunc(int arg1, String bad, String somestr){
        logger.error(String.Format("foo biz {}",bad));
    }

    private void someFunc2(int arg1, String bad, String somestr){
        logger.error("foo " + bad + "bar");
    }

    protected SomeType someFunc3(int arg1, String bad, String somestr){
        logger.error("foo " + bad,arg1);
    }

    protected SomeType someFunc4(int arg1, String bad, String somestr){
        logger.error("foo " + bad + "bar" + arg1 + "...");
    }
}
