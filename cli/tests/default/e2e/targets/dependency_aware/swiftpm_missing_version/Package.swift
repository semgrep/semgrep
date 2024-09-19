// swift-tools-version: 5.9
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "spm",
    products: [
        // Products define the executables and libraries a package produces, making them visible to other packages.
        .library(
            name: "spm",
            targets: ["spm"]),
    ],
    dependencies: [
        .package(url: "https://github.com/apple/swift-collections.git", .upToNextMajor(from: "1.0.0")),
        .package(url: "https://github.com/ReactiveCocoa/ReactiveSwift.git", .upToNextMajor(from: "5.0.0")),
        .package(url: "https://github.com/Carthage/ReactiveTask.git", .upToNextMajor(from: "0.16.0"))
    ],
    targets: [
        // Targets are the basic building blocks of a package, defining a module or a test suite.
        // Targets can depend on other targets in this package and products from dependencies.
        .target(
            name: "spm"),
        .testTarget(
            name: "spmTests",
            dependencies: ["spm"]),
    ]
)
