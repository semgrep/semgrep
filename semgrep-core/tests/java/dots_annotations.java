@Controller
@RequestMapping("/api/test")
public class TestController {
    // ERROR: 
    @RequestMapping(method = RequestMethod.GET)
    @PreAuthorize(Permissions.ADMIN)
    @ResponseBody
    public ResponseEntity<Map<String, Object>> list() {
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
    // ERROR: 
    @RequestMapping(value = "/{name}", method = RequestMethod.POST)
    @PreAuthorize(Permissions.USER)
    @ResponseBody
    public ResponseEntity<Map<String, Object>> load(@PathVariable final String name) throws APIException {
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}

