class Asdf {
    class ReturnObject1 {
        public string Secret;
    }

    public void Test1() {
        // ruleid: tainted-anon-type
        Log(new ReturnObject1() { Secret = GetSecret() });
    }

    public void Test2() {
        // ruleid: tainted-anon-type
        Log(new { Secret = GetSecret() });
    }
}
