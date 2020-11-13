package testcode.xmldecoder;

import java.beans.XMLDecoder;
import java.io.InputStream;

public class XmlDecodeUtil {

    public static void main(String[] args) {
        InputStream in = XmlDecodeUtil.class.getResourceAsStream("/testcode/xmldecoder/obj1.xml");
        XmlDecodeUtil.handleXml(in);
    }

    // ruleid: xml-decoder
    public static Object handleXml(InputStream in) {
        XMLDecoder d = new XMLDecoder(in);
        try {
            Object result = d.readObject(); //Deserialization happen here
            return result;
        }
        finally {
            d.close();
        }
    }

    // ok
    public static Object handleXml1() {
        XMLDecoder d = new XMLDecoder("<safe>XML</safe>");
        try {
            Object result = d.readObject();
            return result;
        }
        finally {
            d.close();
        }
    }

    // ok
    public static Object handleXml2() {
        String strXml = "<safe>XML</safe>";
        XMLDecoder d = new XMLDecoder(strXml);
        try {
            Object result = d.readObject();
            return result;
        }
        finally {
            d.close();
        }
    }

}