plugins {
    id("org.gradle.toolchains.foojay-resolver-convention") version "0.10.0"
}

rootProject.name = "authoring-tutorial"

include("app")
include("lib") // Add lib to the build
