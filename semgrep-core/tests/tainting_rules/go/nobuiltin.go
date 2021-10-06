package fetch

import (
    "context"
    "io/fs"
    "net/http"
    "os"
    "sort"
    "testing"
    "time"

    "golang.org/x/pkgsite/internal"
    "golang.org/x/pkgsite/internal/licenses"
    "golang.org/x/pkgsite/internal/log"
    "golang.org/x/pkgsite/internal/proxy"
    "golang.org/x/pkgsite/internal/proxy/proxytest"
    "golang.org/x/pkgsite/internal/source"
    "golang.org/x/pkgsite/internal/stdlib"
    "golang.org/x/pkgsite/internal/testing/sample"
    "golang.org/x/pkgsite/internal/testing/testhelper"
)

// proxyFetcher is a test helper function that sets up a test proxy, fetches
// a module using FetchModule, and returns fetch result and a license detector.
func proxyFetcher(t *testing.T, withLicenseDetector bool, ctx context.Context, mod *proxytest.Module, fetchVersion string) (*FetchResult, *licenses.Detector) {
    t.Helper()

    modulePath := mod.ModulePath
    version := mod.Version
    if version == "" {
        version = sample.VersionString
    }
    if fetchVersion == "" {
        fetchVersion = version
    }

    proxyClient, teardownProxy := proxytest.SetupTestClient(t, []*proxytest.Module{{
        ModulePath: modulePath,
        Version:    version,
        Files:      mod.Files,
    }})
    defer teardownProxy()
    //OK:
    got := FetchModule(ctx, modulePath, fetchVersion, NewProxyModuleGetter(proxyClient, source.NewClientForTesting()))
    if !withLicenseDetector {
        return got, nil
    }

    //OK:
    d := licenseDetector(ctx, t, modulePath, got.ResolvedVersion, proxyClient)
    return got, d
}

