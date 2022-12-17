class Test {
    @GetMapping("/api/bad3")
    @ResponseBody
    public String bad3(Model model, @RequestParam String input) {
        // ruleid: spring-tainted-code-execution
        @Value("#{input}")
        String var;

        System.out.println(var);
    }
}
