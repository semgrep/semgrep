class Foo {
  //ERROR: match!
  @RequestMapping(method = RequestMethod.GET)
  @PreAuthorize(PermissionUtils.CAN_ADMIN_DO_THIS)
  public
  @ResponseBody
  ResponseEntity<SomeReturnObjectType> 
  list
    (@RequestParam(required = false) Integer someId,
     @RequestParam(required = false) boolean doThing,
     ...) throws Exception { 
    return 1;
  }
}
