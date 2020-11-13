@Controller
public class CookieController {

    @RequestMapping(value = "/cookie1", method = "GET")
    public void setCookie(@RequestParam String value, HttpServletResponse response) {
        // ok
        response.setHeader("Set-Cookie", "key=value; HttpOnly; SameSite=strict");
    }

    @RequestMapping(value = "/cookie2", method = "GET")
    public void setSecureCookie(@RequestParam String value, HttpServletResponse response) {
        // ruleid:cookie-missing-samesite
        response.setHeader("Set-Cookie", "key=value; HttpOnly;");
    }
    
    @RequestMapping(value = "/cookie3", method = "GET")
    public void setSecureHttponlyCookie(@RequestParam String value, HttpServletResponse response) {
        Cookie cookie = new Cookie("cookie", value);
        cookie.setSecure(true);
        cookie.setHttpOnly(true);
        // ruleid:cookie-missing-samesite
        response.addCookie(cookie);
    }

    @RequestMapping(value = "/cookie4", method = "GET")
    public void setEverything(@RequestParam String value, HttpServletResponse response) {
        // ok
        response.setHeader("Set-Cookie", "key=value; HttpOnly; Secure; SameSite=strict");
        response.addCookie(cookie);
    }
}
