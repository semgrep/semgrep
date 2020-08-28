public class PlacesFlag extends ApiMethod implements ApiMethodCallback {

    public enum FlagType {
        INFO_INCORRECT ("info_incorrect"),
        OFFENSIVE      ("offensive"),
        CLOSED         ("closed"),
        DUPLICATE      ("duplicate"),
        NOT_PUBLIC     ("not_public");
    }
}
