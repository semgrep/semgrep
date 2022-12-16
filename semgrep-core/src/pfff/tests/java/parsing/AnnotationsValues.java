@JsonTypeInfo(
  use = JsonTypeInfo.Id.CLASS,
  include = JsonTypeInfo.As.PROPERTY,
  property = "@class")

public interface Entry extends Parcelable {
  public static enum Type {
    PHONE_NUMBER,
    EMAIL,
    ADDRESS,
    DEFAULT
  }

  @JsonIgnore
  public Type getEntryType();
}
