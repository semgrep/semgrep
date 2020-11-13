public class Cls {

    // cf. https://find-sec-bugs.github.io/bugs.htm#INSECURE_SMTP_SSL
    public void sendEmail(String username, String password) {
        Email email = new SimpleEmail();
        email.setHostName("smtp.servermail.com");
        email.setSmtpPort(465);
        email.setAuthenticator(new DefaultAuthenticator(username, password));
        email.setSSLOnConnect(true);
        email.setFrom("user@gmail.com");
        email.setSubject("TestMail");
        email.setMsg("This is a test mail ... :-)");
        email.addTo("foo@bar.com");
        // ruleid:insecure-smtp-connection
        email.send();
    }

    public void sendEmailSafe(String username, String password) {
        Email email = new SimpleEmail();
        email.setHostName("smtp.servermail.com");
        email.setSmtpPort(465);
        email.setAuthenticator(new DefaultAuthenticator(username, password));
        email.setSSLOnConnect(true);
        email.setSSLCheckServerIdentity(true);
        email.setFrom("user@gmail.com");
        email.setSubject("TestMail");
        email.setMsg("This is a test mail ... :-)");
        email.addTo("foo@bar.com");
        email.send();
    }
}
