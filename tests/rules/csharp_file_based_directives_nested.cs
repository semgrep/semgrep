// The SDK only reads `#:` directives at the top of the file, but upstream
// skips them anywhere, so a misplaced one must not break the parse.
#if DEBUG
// ruleid: package-version
#:package Newtonsoft.Json@13.0.1
#endif

class C {
    // ruleid: any-sdk
#:sdk Microsoft.NET.Sdk.Web
    void M() { }
}
