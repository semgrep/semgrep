// cf. https://github.com/oracle/helidon/blob/ab4e308effaa2fe2170a1c312882b2315e66a9af/integrations/cdi/jpa-cdi/src/main/java/io/helidon/integrations/cdi/jpa/JpaExtension.java#L618

package example;

import javax.xml.stream.XMLInputFactory;
import static java.xml.stream.XMLFactoryInput.IS_SUPPORTING_EXTERNAL_ENTITIES;

class GoodXMLInputFactory {
    public void Blah() {
        final XMLInputFactory xmlInputFactory = XMLInputFactory.newFactory();

        // See
        // https://github.com/OWASP/CheatSheetSeries/blob/master/cheatsheets/XML_External_Entity_Prevention_Cheat_Sheet.md#xmlinputfactory-a-stax-parser
        xmlInputFactory.setProperty(XMLInputFactory.SUPPORT_DTD, false);
        // ok
        xmlInputFactory.setProperty("javax.xml.stream.isSupportingExternalEntities", false);
    }
}

class GoodConstXMLInputFactory {
    public void Blah() {
        final XMLInputFactory xmlInputFactory = XMLInputFactory.newFactory();

        // See
        // https://github.com/OWASP/CheatSheetSeries/blob/master/cheatsheets/XML_External_Entity_Prevention_Cheat_Sheet.md#xmlinputfactory-a-stax-parser
        xmlInputFactory.setProperty(XMLInputFactory.SUPPORT_DTD, false);
        // ok
        xmlInputFactory.setProperty(IS_SUPPORTING_EXTERNAL_ENTITIES, false);
    }
}

class BadXMLInputFactory {
    public Blah() {
        // ruleid:xmlinputfactory-possible-xxe
        final XMLInputFactory xmlInputFactory = XMLInputFactory.newFactory();
        xmlInputFactory.setProperty("javax.xml.stream.isSupportingExternalEntities", true);
    }
}

class MaybeBadXMLInputFactory {
    public Blah() {
        // ruleid:xmlinputfactory-possible-xxe
        final XMLInputFactory xmlInputFactory = XMLInputFactory.newFactory();
    }
}  
