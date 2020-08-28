public interface Scope extends javax.inject.Scope {

  /**
   * Returns a provider to one that is scoped according to the rules of the scope.
   *
   * @param provider the unscoped provider
   * @return the scoped provider
   */
  public <T> Provider<T> scope(Provider<T> provider);
}
