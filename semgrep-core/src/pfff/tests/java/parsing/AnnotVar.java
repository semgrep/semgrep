public class HelloWorld {
	public static void main(String[] args) {

        @Language("SQL") String sql = String.format("SELECT field FROM untrusted WHERE nkey='%s'", (String) args[0]);
	}
}
