package example;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class App {
    private static final Logger log = LogManager.getLogger(App.class);

    public static void main(String[] args) {
        String userInput = args.length > 0 ? args[0] : "world";
        log.info("hello " + userInput);
    }
}
