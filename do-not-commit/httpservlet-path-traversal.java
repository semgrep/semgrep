package servlets;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.io.FilenameUtils;

public class Cls extends HttpServlet
{
	private static org.apache.log4j.Logger log = Logger.getLogger(Register.class);

	public void doPost(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException
	{
        // ruleid:httpservlet-path-traversal
        String image = request.getParameter("image");
        File file = new File("static/images/", image);

        if (!file.exists()) {
            log.info(image + " could not be created.");
            response.sendError();
        }

        response.sendRedirect("/index.html");
	}

    public void ok(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException
	{
        // ok
        String image = request.getParameter("image");
        File file = new File("static/images/", FilenameUtils.getName(image));

        if (!file.exists()) {
            log.info(image + " could not be created.");
            response.sendError();
        }

        response.sendRedirect("/index.html");
	}
}
