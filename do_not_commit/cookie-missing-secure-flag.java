@Controller
public class CookieController {

    @RequestMapping(value = "/cookie1", method = "GET")
    public void setCookie(@RequestParam String value, HttpServletResponse response) {
        Cookie cookie = new Cookie("cookie", value);
        // ruleid:cookie-missing-secure-flag
        response.addCookie(cookie);
    }

    @RequestMapping(value = "/cookie2", method = "GET")
    public void setSecureCookie(@RequestParam String value, HttpServletResponse response) {
        Cookie cookie = new Cookie("cookie", value);
        // ok
        cookie.setSecure(true);
        response.addCookie(cookie);
    }
    
    @RequestMapping(value = "/cookie3", method = "GET")
    public void setSecureHttponlyCookie(@RequestParam String value, HttpServletResponse response) {
        Cookie cookie = new Cookie("cookie", value);
        // ok
        cookie.setSecure(true);
        cookie.setHttpOnly(true);
        response.addCookie(cookie);
    }

    @RequestMapping(value = "/cookie4", method = "GET")
    public void explicitDisable(@RequestParam String value, HttpServletResponse response) {
        Cookie cookie = new Cookie("cookie", value);
        // ruleid:cookie-missing-secure-flag
        cookie.setSecure(false);
        cookie.setHttpOnly(false);
        response.addCookie(cookie);
    }

    // test case cf. https://github.com/Dreampie/Resty//blob/9ef059c065d1894c79e7d69c150e588a61eb1cd5/resty-common/src/main/java/cn/dreampie/common/http/HttpResponse.java#L69
    public Response addCookie(String name, String value, int expiration, boolean httpOnly) {
    Cookie existingCookie = HttpRequest.getCookie(request.getCookies(), name);
    if (existingCookie != null) {
      if (Constant.cookiePath.equals(existingCookie.getPath())
          || existingCookie.getPath() == null // in some cases cookies set on path '/' are returned with a null path
          ) {
        // update existing cookie
        existingCookie.setPath(Constant.cookiePath);
        existingCookie.setValue(value);
        existingCookie.setMaxAge(expiration);
        if (Constant.cookieHttpOnly) {
          setHttpOnly(existingCookie);
        }
        existingCookie.setSecure(Constant.cookieSecure);
        if (Constant.cookieDomain != null) {
          existingCookie.setDomain(Constant.cookieDomain);
        }
        // ok
        response.addCookie(existingCookie);
      } else {
        // we have an existing cookie on another path: clear it, and add a new cookie on root path
        existingCookie.setValue("");
        existingCookie.setMaxAge(0);
        // ok
        response.addCookie(existingCookie);

        Cookie c = new Cookie(name, value);
        c.setPath(Constant.cookiePath);
        c.setMaxAge(expiration);
        if (Constant.cookieHttpOnly) {
          setHttpOnly(existingCookie);
        }
        c.setSecure(Constant.cookieSecure);
        if (Constant.cookieDomain != null) {
          c.setDomain(Constant.cookieDomain);
        }
        // ok
        response.addCookie(c);
      }
    } else {
      Cookie c = new Cookie(name, value);
      c.setPath(Constant.cookiePath);
      c.setMaxAge(expiration);
      if (Constant.cookieHttpOnly) {
        setHttpOnly(c);
      }
      c.setSecure(Constant.cookieSecure);
      if (Constant.cookieDomain != null) {
        c.setDomain(Constant.cookieDomain);
      }
      // ok
      response.addCookie(c);
    }
    return this;
  }

  public Response clearCookie(String cookie) {
    Cookie existingCookie = HttpRequest.getCookie(request.getCookies(), cookie);
    if (existingCookie != null) {
      existingCookie.setPath(Constant.cookiePath);
      existingCookie.setValue("");
      existingCookie.setMaxAge(0);
      // ok
      response.addCookie(existingCookie);
    }
    return this;
  }

}