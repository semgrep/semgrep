public @interface TestEx {

    String featureIds()[] default {};

    String featureId() default "";
}
