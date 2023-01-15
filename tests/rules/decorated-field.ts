class Foo {
    // ruleid: decorated-method
    @Input() gameMarkdown: string;
}

class Bar {
    // ruleid: decorated-method
    @Input gameMarkdown: string;
}

class Baz {
    @Input gameMarkdown(): string {
        return 2
    };
}