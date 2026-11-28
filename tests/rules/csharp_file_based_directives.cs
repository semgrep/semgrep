#!/usr/bin/env dotnet
// ruleid: package-version, any-package
#:package Newtonsoft.Json@13.0.1
// ruleid: any-package
#:package Humanizer@2.14.1
// ruleid: any-sdk
#:sdk Microsoft.NET.Sdk.Web
#:property LangVersion=preview
#:project ../Lib/Lib.csproj

// No rule matches below; `old-version` (1.0) must not match 13.0.1 above.
class C { }
