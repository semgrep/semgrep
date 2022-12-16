class Foo {
  void main() {

      ImmutableMap<BuildTarget, NativeLinkableGroup> roots =
          NativeLinkableGroups.getNativeLinkableRoots(
              buildDeps,
              (Function<? super BuildRule, Optional<Iterable<? extends BuildRule>>>)
                   r -> r instanceof NdkLibrary ? Optional.of(r.getBuildDeps()) : Optional.empty());

    ImmutableList<Callable<Unit>> callables =
        dxSteps
            .map(
                steps ->
                    (Callable<Unit>)
                         () -> {
                          for (Step step : steps) {
                            StepRunner.runStep(context, step, Optional.of(buildTarget));
                          }
                          return Unit.UNIT;
                        })
            .collect(ImmutableList.toImmutableList());

  }
}
