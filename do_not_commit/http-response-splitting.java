@Controller
@RequestMapping("/api/test")
public class TestController {

    @RequestMapping(method = RequestMethod.GET)
    @PreAuthorize(Permissions.ADMIN)
    @ResponseBody
    public void list(HttpServletRequest request, HttpServletResponse response) {
        // ruleid:http-response-splitting
        String author = request.getParameter(AUTHOR_PARAMETER);
        Cookie cookie = new Cookie("author", author);
        response.addCookie(cookie);
    }

    @RequestMapping(value = "/{name}", method = RequestMethod.POST)
    @PreAuthorize(Permissions.USER)
    @ResponseBody
    public void load(@PathVariable final String name, HttpServletResponse response) throws APIException {
        // ruleid:http-response-splitting
        Cookie cookie = new Cookie("author", name);
        response.addCookie(cookie);
    }

    private Response safe(String name, Response response) {
        // ok
        Cookie cookie = new Cookie("author", name);
        response.addCookie(cookie);
        return response;
    }
    
    @RequestMapping(value = "/{name}/{book}", method = RequestMethod.POST)
    @PreAuthorize(Permissions.USER)
    @ResponseBody
    public void loadBook(@PathVariable final String name, @PathVariable final String book, HttpServletResponse response) throws APIException {
        AuthorObj author = AuthorObj.getAuthor(name, book);
        // ok
        Cookie cookie = new Cookie("sess", "1234");
        response.addCookie(cookie);
    }
}
