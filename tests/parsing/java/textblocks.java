//@Controller
//@RequestMapping("/api/test")
public class ExampleController {

    //@RequestMapping(method = RequestMethod.GET)
    //@Authorize(Permissions.ADMIN)
    //@ResponseBody
    //public ResponseEntity<Map<String, Object>> list() {
    //    return new ResponseEntity<>(result, HttpStatus.OK);
    //}
    //
    //@RequestMapping(method = RequestMethod.GET)
    //@ResponseBody
    public ResponseEntity<Map<String, Object>> unauth() {
	//Java 15 Text Blocks!
        String var = """ 
                        JAVA 15 string
                    """;
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    //@RequestMapping(value = "/{name}", method = RequestMethod.POST)
    //@Authorize(Permissions.USER)
    //@ResponseBody
    //public ResponseEntity<Map<String, Object>> load(@PathVariable final String name) throws APIException {
    //    return new ResponseEntity<>(result, HttpStatus.OK);
    //}
}
