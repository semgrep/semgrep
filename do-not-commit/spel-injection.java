package testcode.script;


import org.springframework.expression.Expression;
import org.springframework.expression.ExpressionParser;
import org.springframework.expression.common.TemplateAwareExpressionParser;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.expression.spel.support.StandardEvaluationContext;

public class SpelSample {

    private static PersonDTO TEST_PERSON = new PersonDTO("Benoit", "Doudou");

    // ruleid: spel-injection
    public static void parseExpressionInterface1(String property) {
        ExpressionParser parser = new SpelExpressionParser();
        StandardEvaluationContext testContext = new StandardEvaluationContext(TEST_PERSON);
        Expression exp2 = parser.parseExpression(property+" == 'Benoit'");
        String dynamicValue = exp2.getValue(testContext, String.class);
        System.out.println("exp2="+dynamicValue);
    }

    // ok
    public static void parseExpressionInterface2(String property) {
        ExpressionParser parser = new SpelExpressionParser();
        Expression exp1 = parser.parseExpression("'safe expression'");
        String constantValue = exp1.getValue(String.class);
        System.out.println("exp1="+constantValue);
    }

    // ruleid: spel-injection
    public static void parseSpelExpression3(String property) {
        SpelExpressionParser parser = new SpelExpressionParser();
        StandardEvaluationContext testContext = new StandardEvaluationContext(TEST_PERSON);
        Expression exp2 = parser.parseExpression(property+" == 'Benoit'");
        String dynamicValue = exp2.getValue(testContext, String.class);
        System.out.println("exp2=" + dynamicValue);
    }

    // ok
    public static void parseSpelExpression4(String property) {
        SpelExpressionParser parser = new SpelExpressionParser();
        Expression exp1 = parser.parseExpression("'safe expression'");
        String constantValue = exp1.getValue(String.class);
        System.out.println("exp1="+constantValue);
    }

    // ok
    public static void parseTemplateAwareExpression1(String property) {
        TemplateAwareExpressionParser parser = new SpelExpressionParser();
        Expression exp1 = parser.parseExpression("'safe expression'");
        String constantValue = exp1.getValue(String.class);
        System.out.println("exp1="+constantValue);
    }

    // ruleid: spel-injection
    public static void parseTemplateAwareExpression2(String property) {
        TemplateAwareExpressionParser parser = new SpelExpressionParser();
        StandardEvaluationContext testContext = new StandardEvaluationContext(TEST_PERSON);
        Expression exp2 = parser.parseExpression(property+" == 'Benoit'");
        String dynamicValue = exp2.getValue(testContext, String.class);
        System.out.println("exp2="+dynamicValue);
    }

    public static void main(String[] args) {
        //Expected use case..
        parseExpressionInterface("firstName");
        //Malicious use case..
        parseExpressionInterface("T(java.lang.Runtime).getRuntime().exec('calc.exe')");
    }

    static class PersonDTO {
        public final String firstName;
        public final String lastName;

        public PersonDTO(String firstName, String lastName) {
            this.firstName = firstName;
            this.lastName = lastName;
        }
    }
}
