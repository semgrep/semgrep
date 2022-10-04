struct Foo {
    bar: u32,
    baz: String
}


impl Foo {
    pub fn new() -> Self {
        Foo {
            bar: 42,
            baz: "qux".into_string()
        }
    }

    pub fn test(&self, bar: u32) -> u32 {
        if self.bar < 24 {
            self.bar + bar
        } else {
            self.bar - bar
        }
    }
}
