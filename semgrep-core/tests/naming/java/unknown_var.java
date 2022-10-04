public class tableConcatStatements {
    public void tableConcat() {
        // ok
        stmt.execute("DROP TABLE " + tableName);
        stmt.execute(String.format("CREATE TABLE %s", tableName));
    }
}
