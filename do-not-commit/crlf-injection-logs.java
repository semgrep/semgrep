package com.vogella.logger.test;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.vogella.logger.MyLogger;

// ruleid: crlf-injection-logs
public class TestLog1 {
  private final static Logger log = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		String param = request.getParameter("param");
		log.info("foo"+param+"bar");
		response.getWriter().append("Served at: ").append(request.getContextPath());
	}
}

// ruleid: crlf-injection-logs
public class TestLog2 {
  private final static Logger log = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

  @Override
  public void doFilter(ServletRequest request, ServletResponse response,
    FilterChian chain) throws IOException, ServletException {
      HttpServletResponse httpServletResponse = (HttpServletResponse) response;
			String param = request.getParameter("param");
    	log.log(log.getLevel(), "foo"+param);
  }
}

// ruleid: crlf-injection-logs
public class TestLog3 {
  private final static Logger log = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

  protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		log.info("foo"+request.getParameter("param"));
		response.getWriter().append("Served at: ").append(request.getContextPath());
	}
}

// ruleid: crlf-injection-logs
public class TestLog4 {
  private final static Logger log = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

  @Override
  public void doFilter(ServletRequest request, ServletResponse response,
    FilterChian chain) throws IOException, ServletException {
      HttpServletRequest httpServletReq = (HttpServletRequest) request;
			String param = httpServletReq.getParameter("param");
    	log.log(log.getLevel(), param);
  }
}

public class TestLog5 {

	// ruleid: crlf-injection-logs
  @Override
  public void doFilter(ServletRequest request, ServletResponse response,
    FilterChian chain) throws IOException, ServletException {
			Logger  log = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);
      HttpServletRequest httpServletReq = (HttpServletRequest) request;
			String param = httpServletReq.getParameter("foo");
    	log.log(log.getLevel(), param+"bar");
  }
}

// ok
public class OkTestLog1 {
  private final static NotLogger log = new NorLogger();

  @Override
  public void doFilter(ServletRequest request, ServletResponse response,
    FilterChian chain) throws IOException, ServletException {
      HttpServletRequest httpServletReq = (HttpServletRequest) request;
			String param = httpServletReq.getParameter("param");
    	log.info(param);
  }
}

public class OkTestLog2 {
  // ok
  @Override
  public void doFilter(ServletRequest request, ServletResponse response,
    FilterChian chain) throws IOException, ServletException {
			Logger log = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);
      HttpServletRequest httpServletReq = (HttpServletRequest) request;
			String param = "foobar";
    	log.log(log.getLevel(), param);
  }
}
