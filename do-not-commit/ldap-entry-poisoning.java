public class Cls {

    public void ldapSearchEntryPoison(Environment env) {
        DirContext ctx = new InitialDirContext();

        // ruleid:ldap-entry-poisoning
        ctx.search(query, filter, new SearchControls(scope, countLimit, timeLimit, attributes,
            true, //Enable object deserialization if bound in directory
            deref));
    }

    public void ldapSearchEntryPoisonViaSetter(Environment env) {
        DirContext ctx = new InitialDirContext();
        // ruleid:ldap-entry-poisoning
        SearchControls ctrls = new SearchControls();
        ctrls.setReturningObjFlag(true);
    }

    public void ldapSearchSafe(Environment env) {
        DirContext ctx = new InitialDirContext();
        ctx.search(query, filter,
            new SearchControls(scope, countLimit, timeLimit, attributes,
            false, //Disable
            deref));
    }
}
