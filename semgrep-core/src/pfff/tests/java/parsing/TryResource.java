class Foo {
  public static void main() {

    for (Path jarPath : jarPaths) {
      try (JarFile jarFile = new JarFile(jarPath.toFile())) {
        loadClassNodes(jarFile, builder);
      } catch (IOException e) {
        throw new RuntimeException(e);
      }

      try (JarFile jarFile = new JarFile(jarPath.toFile());
           X a = 1;) {
        loadClassNodes(jarFile, builder);
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
    }

  }
}
