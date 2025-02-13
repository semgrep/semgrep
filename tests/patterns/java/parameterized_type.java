public class A {
  public void SpecificSerializer(Class<T> clazz)
  {
      // MATCH:
      new SpecificDatumReader<T>(clazz);
      // MATCH:
      new SpecificDatumReader(clazz);
  }
}