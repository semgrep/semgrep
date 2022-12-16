class Foo {
  public void main() {

    List<Path> compiledResourcePaths =
        Lists.reverse(compileRules).stream()
            .map(Aapt2Compile::getSourcePathToOutput)
            .map(context.getSourcePathResolver()::getRelativePath)
            .collect(Collectors.toList());

    this.devicesSupplier = MoreSuppliers.memoize(this::getDevicesImpl);

    proguardTextFilesPath.map(buildContext.getSourcePathResolver()::getRelativePath);
  }

}
