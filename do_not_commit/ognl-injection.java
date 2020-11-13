package testcode.script.ognl;

import com.opensymphony.xwork2.ognl.OgnlReflectionProvider;

import javax.management.ReflectionException;
import java.beans.IntrospectionException;
import java.util.HashMap;
import java.util.Map;

public class OgnlReflectionProviderSample {

    // ruleid: ognl-injection
    public void unsafeOgnlReflectionProvider(String input, OgnlReflectionProvider reflectionProvider, Class type) throws IntrospectionException, ReflectionException {
        reflectionProvider.getGetMethod(type, input);
    }

    // ruleid: ognl-injection
    public void unsafeOgnlReflectionProvider1(String input, ReflectionProvider reflectionProvider) throws IntrospectionException, ReflectionException {
        reflectionProvider.getValue(input, null, null);
    }

    // ruleid: ognl-injection
    public void unsafeOgnlReflectionProvider2(String input, OgnlUtil reflectionProvider) throws IntrospectionException, ReflectionException {
        reflectionProvider.setValue(input, null, null,null);
    }

    // ruleid: ognl-injection
    public void unsafeOgnlReflectionProvider3(String input, OgnlTextParser reflectionProvider) throws IntrospectionException, ReflectionException {
        reflectionProvider.evaluate( input );
    }

    // ok
    public void safeOgnlReflectionProvider1(OgnlReflectionProvider reflectionProvider, Class type) throws IntrospectionException, ReflectionException {
        String input = "thisissafe";
        reflectionProvider.getGetMethod(type, input);
    }

    // ok
    public void safeOgnlReflectionProvider2(OgnlReflectionProvider reflectionProvider, Class type) throws IntrospectionException, ReflectionException {
        reflectionProvider.getField(type, "thisissafe");
    }

}
