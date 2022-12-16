public class JMAutogen {

  private static Class<?> TAG = JMAutogen.class;

  @Retention(RetentionPolicy.RUNTIME)
  @Target(ElementType.FIELD)
  public @interface InferredType {
    public String jsonFieldName();
  }
}
