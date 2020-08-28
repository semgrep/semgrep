public class PlacesFlag extends ApiMethod implements ApiMethodCallback {

    public enum FlagType {
        INFO_INCORRECT ("info_incorrect"),
        OFFENSIVE      ("offensive"),
        CLOSED         ("closed"),
        DUPLICATE      ("duplicate"),
        NOT_PUBLIC     ("not_public");

        public static final String ARG_NAME = "flag";

        public final String mArgVal;

        FlagType(String argVal) {
            this.mArgVal = argVal;
        }
    }
}
