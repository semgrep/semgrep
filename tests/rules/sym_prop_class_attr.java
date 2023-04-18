package example;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.ParserConfigurationException;

class BadDocumentBuilderFactoryStatic1 {

    private static DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();

    public void doSomething(){
        //ruleid:documentbuilderfactory-disallow-doctype-decl-missing
        dbf.newDocumentBuilder();
    }

}

class BadDocumentBuilderFactoryStatic2 {

    private static DocumentBuilderFactory dbf;

    static {
        dbf = DocumentBuilderFactory.newInstance();
    }

    public void doSomething(){
        //ruleid:documentbuilderfactory-disallow-doctype-decl-missing
        dbf.newDocumentBuilder();
    }

}
