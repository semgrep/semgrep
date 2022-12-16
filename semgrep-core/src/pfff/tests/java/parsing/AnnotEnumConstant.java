@Incubating
public enum SerializableMode {

    /**
     * No serialization.
     */
    NONE,

    /**
     * Basic serializable mode for mock objects. Introduced in Mockito 1.8.1.
     */
    BASIC,

    /**
     * Useful if the mock is deserialized in a different classloader / vm.
     */
     @Incubating
    ACROSS_CLASSLOADERS
}
