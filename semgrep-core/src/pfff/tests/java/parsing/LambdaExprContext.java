class Foo {

  void main() {

    shouldContainNode = node -> true;

    this.normalizer =
        arg -> {
          ImmutableSet<String> aliasValues =
              AliasConfig.from(buckConfig).getBuildTargetForAliasAsString(arg);
          if (!aliasValues.isEmpty()) {
            return aliasValues;
          } else {
            return ImmutableSet.of(normalizeBuildTargetIdentifier(arg));
          }
        };

    this.rDotJavaPackageSupplier =
         () -> {
          String rDotJavaPackage1 = AndroidResource.this.rDotJavaPackage.get();
          if (rDotJavaPackage1 != null) {
            return rDotJavaPackage1;
          } else {
            throw new RuntimeException(
                "rDotJavaPackage for "
                    + AndroidResource.this.getBuildTarget()
                    + " was requested before it was made available.");
          }
        };
    this.isGrayscaleImageProcessingEnabled = isGrayscaleImageProcessingEnab;



    CodeSignIdentity identity =
            codesignIdentitySubjectName
                .map(id -> CodeSignIdentity.ofAdhocSignedWithSubjectCommonName(id))
                .orElse(CodeSignIdentity.AD_HOC);
         codeSignIdentitySupplier = () -> identity;
 }

}
