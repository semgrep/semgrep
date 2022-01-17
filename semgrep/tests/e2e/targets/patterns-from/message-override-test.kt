public class CookieController {
    public fun setCookie(value: String, response: HttpServletResponse) {
        var cookie: Cookie = Cookie("cookie", value)
        // ruleid:cookie-missing-secure-flag
        response.addCookie(cookie)
    }

    public fun setSecureCookie(@RequestParam value: String, response: HttpServletResponse) {
       var cookie: Cookie = Cookie("cookie", value)
        // ok:cookie-missing-secure-flag
        cookie.setSecure(true)
        response.addCookie(cookie)
    }

    public fun setSecureHttponlyCookie(@RequestParam value: String, response: HttpServletResponse) {
       var cookie: Cookie = Cookie("cookie", value)
        // ok:cookie-missing-secure-flag
        cookie.setSecure(true)
        cookie.setHttpOnly(true)
        response.addCookie(cookie)
    }

    public fun explicitDisable(@RequestParam value: String, response: HttpServletResponse) {
       var cookie: Cookie = Cookie("cookie", value)
        // ruleid:cookie-missing-secure-flag
        cookie.setSecure(false)
        cookie.setHttpOnly(false)
        response.addCookie(cookie)
    }

    // test case cf. https://github.com/Dreampie/Resty//blob/9ef059c065d1894c79e7d69c150e588a61eb1cd5/resty-common/src/main/java/cn/dreampie/common/http/HttpResponse.java#L69
    public fun addCookie(name: String, value: String, expiration: int, httpOnly: boolean) {
    var existingCookie: Cookie = HttpRequest.getCookie(request.getCookies(), name)
    if (existingCookie != null) {
      if (Constant.cookiePath.equals(existingCookie.getPath())
          || existingCookie.getPath() == null // in some cases cookies set on path '/' are returned with a null path
          ) {
        // update existing cookie
        existingCookie.setPath(Constant.cookiePath)
        existingCookie.setValue(value)
        existingCookie.setMaxAge(expiration)
        if (Constant.cookieHttpOnly) {
          setHttpOnly(existingCookie)
        }
        existingCookie.setSecure(Constant.cookieSecure)
        if (Constant.cookieDomain != null) {
          existingCookie.setDomain(Constant.cookieDomain)
        }
        // ok:cookie-missing-secure-flag
        response.addCookie(existingCookie)
      } else {
        // we have an existing cookie on another path: clear it, and add a new cookie on root path
        existingCookie.setValue("")
        existingCookie.setMaxAge(0)
        // ok:cookie-missing-secure-flag
        response.addCookie(existingCookie)

        var c: Cookie = Cookie(name, value)
        c.setPath(Constant.cookiePath)
        c.setMaxAge(expiration)
        if (Constant.cookieHttpOnly) {
          setHttpOnly(existingCookie)
        }
        c.setSecure(Constant.cookieSecure)
        if (Constant.cookieDomain != null) {
          c.setDomain(Constant.cookieDomain)
        }
        // ok:cookie-missing-secure-flag
        response.addCookie(c)
      }
    } else {
      var c: Cookie = Cookie(name, value)
      c.setPath(Constant.cookiePath)
      c.setMaxAge(expiration)
      if (Constant.cookieHttpOnly) {
        setHttpOnly(c)
      }
      c.setSecure(Constant.cookieSecure)
      if (Constant.cookieDomain != null) {
        c.setDomain(Constant.cookieDomain)
      }
      // ok:cookie-missing-secure-flag
      response.addCookie(c)
    }
    return this
  }

  public fun clearCookie(cookie: String) {
    var existingCookie: Cookie = HttpRequest.getCookie(request.getCookies(), cookie)
    if (existingCookie != null) {
      existingCookie.setPath(Constant.cookiePath)
      existingCookie.setValue("")
      existingCookie.setMaxAge(0)
      // ok:cookie-missing-secure-flag
      response.addCookie(existingCookie)
    }
    return this
  }

}
