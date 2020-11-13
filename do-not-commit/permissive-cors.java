package foolet;

import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Servlet implementation class SuperWebFlet
 */
@WebServlet("/SuperWebFlet")
public class SuperWebFlet extends HttpServlet {

    public SuperWebFlet() {
        // Auto-generated constructor stub
    }

    @Override
    public void doFilter(ServletRequest request, ServletResponse response,
                         FilterChain chain) throws IOException, ServletException {
        // ruleid: permissive-cors
        HttpServletResponse res = (HttpServletResponse) response;
        res.addHeader("Access-Control-Allow-Origin", "*");
        chain.doFilter(request, response);
    }

    // ruleid: permissive-cors
    @GetMapping({"", "/"})
    @PreAuthorize("hasPermission('User', 'read')")
    public List index(HttpServletRequest request, HttpServletResponse response) {
        response.addHeader("access-control-allow-origin", "*");
        return page.getContent().stream().map((item) -> {
            Map<String, Object> ret = new HashMap();
            ret.put("createdAt", item.getCreatedAt());
            return ret;
        }).collect(Collectors.toList());
    }

    // ruleid: permissive-cors
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        try {
            response.setCharacterEncoding("UTF-8");
            response.setContentType("text/html; charset=UTF-8");
            response.setHeader("Access-Control-Allow-Origin", "Null");
            boolean ok = "OK".equals(ibookDbStatus);
            if (!ok) {
                response.setStatus(500);
            }
        }
        catch (RuntimeException | IOException e) {
            logger.log(Level.SEVERE, "RQ[HEALT] -> "+e.toString(), e);
            throw e;
        }
    }

    // ruleid: permissive-cors
    public void setErrorsResponse(Errors errors, HttpStatus responseHttpStatus, HttpServletRequest request, HttpServletResponse response) throws IOException {
        response.setStatus(responseHttpStatus.value());
        HttpResponseData responseData = getResponseData(errors, request);
        if (responseData != null) {
            response.addHeader("access-control-allow-origin", "*");
            response.getWriter().write(responseData.getBody());
        }
    }

    // ruleid: permissive-cors
    public static void write(HttpServletResponse response, Object o) throws Exception {
        response.setContentType("text/html;charset=utf-8");
        response.addHeader("Access-Control-Allow-Origin", "*.test.com");
        PrintWriter out = response.getWriter();
        out.println(o.toString());
        out.flush();
        out.close();
    }

    @GetMapping("/response-entity-builder-with-http-headers")
    public ResponseEntity<String> usingResponseEntityBuilderAndHttpHeaders() {
        // ruleid: permissive-cors
        HttpHeaders responseHeaders = new HttpHeaders();
        responseHeaders.set("Access-Control-Allow-Origin", "*");

        return ResponseEntity.ok()
        .headers(responseHeaders)
        .body("Response with header using ResponseEntity");
    }

    // ruleid: permissive-cors
    @GetMapping("/server-http-response")
    public Mono<String> usingServerHttpResponse(ServerHttpResponse response) {
        response.getHeaders().add("Access-Control-Allow-Origin", "*");
        return Mono.just("Response with header using ServerHttpResponse");
    }

    @GetMapping("/response-entity")
    public Mono<ResponseEntity<String>> usingResponseEntityBuilder() {
        String responseBody = "Response with header using ResponseEntity (builder)";
        // ruleid: permissive-cors
        return Mono.just(ResponseEntity.ok()
        .header("Access-Control-Allow-Origin", "*")
        .body(responseBody));
    }

    public Mono<ServerResponse> useHandler(final ServerRequest request) {
    // ruleid: permissive-cors
     return ServerResponse.ok()
        .header("Access-Control-Allow-Origin", "null")
        .body(Mono.just("Response with header using Handler"),String.class);
    }

    // ruleid: permissive-cors
    @Override
    public Mono<Void> filter(ServerWebExchange exchange, WebFilterChain chain) {
        exchange.getResponse()
            .getHeaders()
            .add("Access-Control-Allow-Origin", "*.some.domain");
        return chain.filter(exchange);
    }

    // ok
    public void setErrorsResponse1(Errors errors, HttpStatus responseHttpStatus, HttpServletRequest request, HttpServletResponse response) throws IOException {
        response.addHeader("Foo", "Bar");
        response.getWriter().write(responseData.getBody());
    }

    // ok
    @GetMapping("/ok-ok")
    public Mono<String> usingServerHttpResponse1(ServerHttpResponse response) {
        response.getHeaders().add("Foo", "Bar");
        return Mono.just("Response with header using ServerHttpResponse");
    }

    @GetMapping("/ok-ok-ok")
    public ResponseEntity<String> usingResponseEntityBuilderAndHttpHeaders1() {
        // ok
        HttpHeaders responseHeaders = new HttpHeaders();
        responseHeaders.set("Foo", "Bar");

        return ResponseEntity.ok()
        .headers(responseHeaders)
        .body("Response with header using ResponseEntity");
    }

}
