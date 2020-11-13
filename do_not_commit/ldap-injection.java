package testcode.ldap;

import com.sun.jndi.ldap.LdapCtx;
import javax.naming.Context;
import javax.naming.InvalidNameException;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.directory.DirContext;
import javax.naming.directory.InitialDirContext;
import javax.naming.directory.SearchControls;
import javax.naming.directory.SearchResult;
import javax.naming.event.EventDirContext;
import javax.naming.ldap.InitialLdapContext;
import javax.naming.ldap.LdapContext;
import javax.naming.ldap.LdapName;
import java.util.Properties;

public class JndiLdapAdditionalSignature {
    
    // ruleid: ldap-injection
    public static void moreLdapInjections(String input) throws NamingException {
        Properties props = new Properties();
        props.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory");
        props.put(Context.PROVIDER_URL, "ldap://ldap.example.com");
        props.put(Context.REFERRAL, "ignore");

        SearchControls ctrls = new SearchControls();
        ctrls.setReturningAttributes(new String[]{"givenName", "sn"});
        ctrls.setSearchScope(SearchControls.SUBTREE_SCOPE);

        DirContext context1 = new InitialDirContext(props);

        NamingEnumeration<SearchResult> answers;
        answers = context1.search(new LdapName("dc=People,dc=example,dc=com"), "(uid=" + input + ")", ctrls);
    }

    // ruleid: ldap-injection
    public static void moreLdapInjections1(String input) throws NamingException {
        Properties props = new Properties();
        props.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory");
        props.put(Context.PROVIDER_URL, "ldap://ldap.example.com");
        props.put(Context.REFERRAL, "ignore");

        SearchControls ctrls = new SearchControls();
        ctrls.setReturningAttributes(new String[]{"givenName", "sn"});
        ctrls.setSearchScope(SearchControls.SUBTREE_SCOPE);

        InitialDirContext context2 = new InitialDirContext(props);

        NamingEnumeration<SearchResult> answers;
        answers = context2.search(new LdapName("dc=People,dc=example,dc=com"), "(uid=" + input + ")", new Object[0], ctrls);
    }

    // ruleid: ldap-injection
    public static void moreLdapInjections2(String input) throws NamingException {
        Properties props = new Properties();
        props.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory");
        props.put(Context.PROVIDER_URL, "ldap://ldap.example.com");
        props.put(Context.REFERRAL, "ignore");

        SearchControls ctrls = new SearchControls();
        ctrls.setReturningAttributes(new String[]{"givenName", "sn"});
        ctrls.setSearchScope(SearchControls.SUBTREE_SCOPE);

        InitialLdapContext context3 = new InitialLdapContext();
        LdapContext        context4 = new InitialLdapContext();

        NamingEnumeration<SearchResult> answers;
        answers = context3.search("dc=People,dc=example,dc=com", "(uid=" + input + ")", ctrls);
    }

    // ruleid: ldap-injection
    public static void moreLdapInjections3(String input) throws NamingException {
        Properties props = new Properties();
        props.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory");
        props.put(Context.PROVIDER_URL, "ldap://ldap.example.com");
        props.put(Context.REFERRAL, "ignore");

        SearchControls ctrls = new SearchControls();
        ctrls.setReturningAttributes(new String[]{"givenName", "sn"});
        ctrls.setSearchScope(SearchControls.SUBTREE_SCOPE);

        LdapContext context4 = new InitialLdapContext();

        NamingEnumeration<SearchResult> answers;
        answers = context4.search("dc=People,dc=example,dc=com", "(uid=" + input + ")", new Object[0], ctrls);
    }

    // ruleid: ldap-injection
    public void ldapInjectionSunApi5(String input) throws NamingException {
        Properties props = new Properties();
        props.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory");
        props.put(Context.PROVIDER_URL, "ldap://ldap.example.com");
        props.put(Context.REFERRAL, "ignore");

        SearchControls ctrls = new SearchControls();
        ctrls.setReturningAttributes(new String[]{"givenName", "sn"});
        ctrls.setSearchScope(SearchControls.SUBTREE_SCOPE);

        LdapCtx context5 = new InitialDirContext(props);

        NamingEnumeration<SearchResult> answers;
        answers = context5.search("dc=People,dc=example,dc=com", "(uid=" + input + ")", new Object[0], ctrls);
    }

    // ruleid: ldap-injection
    public void ldapInjectionSunApi6(String input) throws NamingException {
        Properties props = new Properties();
        props.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory");
        props.put(Context.PROVIDER_URL, "ldap://ldap.example.com");
        props.put(Context.REFERRAL, "ignore");

        SearchControls ctrls = new SearchControls();
        ctrls.setReturningAttributes(new String[]{"givenName", "sn"});
        ctrls.setSearchScope(SearchControls.SUBTREE_SCOPE);

        EventDirContext context6 = new InitialDirContext(props);

        NamingEnumeration<SearchResult> answers;
        answers = context6.search("dc=People,dc=example,dc=com", "(uid=" + input + ")", new Object[0], ctrls);
    }

    // ok
    public static void moreLdapInjections4(String input) throws NamingException {
        Properties props = new Properties();
        props.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory");
        props.put(Context.PROVIDER_URL, "ldap://ldap.example.com");
        props.put(Context.REFERRAL, "ignore");

        SearchControls ctrls = new SearchControls();
        ctrls.setReturningAttributes(new String[]{"givenName", "sn"});
        ctrls.setSearchScope(SearchControls.SUBTREE_SCOPE);

        DirContext context1 = new InitialDirContext(props);

        NamingEnumeration<SearchResult> answers;
        //False positive
        answers = context1.search(new LdapName("dc=People,dc=example,dc=com"), "(uid=bob)", new Object[0], ctrls);
    }


}
