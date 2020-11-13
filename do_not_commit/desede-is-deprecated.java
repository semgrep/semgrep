package servlets;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

public class Cls extends HttpServlet
{
	private static org.apache.log4j.Logger log = Logger.getLogger(Register.class);

    // cf. https://find-sec-bugs.github.io/bugs.htm#TDES_USAGE
	protected void danger(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        // ruleid: desede-is-deprecated
        Cipher c = Cipher.getInstance("DESede/ECB/PKCS5Padding");
        c.init(Cipher.ENCRYPT_MODE, k, iv);
        byte[] cipherText = c.doFinal(plainText);
    }

    protected void ok(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        // ok
        Cipher c = Cipher.getInstance("AES/GCM/NoPadding");
        c.init(Cipher.ENCRYPT_MODE, k, iv);
        byte[] cipherText = c.doFinal(plainText);
    }
} 