package parsers.DocumentBuilderFactory;

import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.*;

class Foobar {

    private static DocumentBuilderFactory dbFactory;

    static {
        dbFactory = DocumentBuilderFactory.newInstance();
    }

    public void todoExample(File input) {
        //ruleid: test
        DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
        dBuilder.parse(input);
    }
}
