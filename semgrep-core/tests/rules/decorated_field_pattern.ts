class Foo {
    // ruleid: decorated-field-pattern 
    @Input() bar: string;
    // ruleid: decorated-field-pattern 
    @Input bar: string;

    @Input bar(): string {
        return 5
    }
}