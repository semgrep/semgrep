import org.cyclonedx.parsers.XmlParser

class Poc {
    companion object {
        fun main(args: Array<String>) {
            // MATCH:
            XmlParser().parse("hi")

            val bomParser = XmlParser()
            // MATCH:
            bomParser.parse("there")

            val parser : XmlParser = XmlParser()
            // MATCH:
            parser.parse("!")
        }
    }
}
