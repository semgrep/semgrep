@BuckStyleImmutable
@Value.Immutable(copy = true)
interface AbstractAndroidAppModularityDescriptionArg
    extends BuildRuleArg, HasDeclaredDeps, HasApplicationModuleBlacklist {
  Map<String, List<BuildTarget>> getApplicationModuleConfigs();

  Optional<Map<String, List<String>>> getApplicationModuleDependencies();

  @Hint(isDep = false)
  ImmutableSet<BuildTarget> getNoDx();

  @Value.Default
     default boolean getShouldIncludeClasses() {
    return true;
  }

  @Value.Default
  default boolean getShouldIncludeLibraries() {
    return false;
  }
}
