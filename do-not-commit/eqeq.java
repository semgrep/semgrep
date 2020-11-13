class Bar {
    void main() {
        boolean myBoolean;

        //myBoolean == myBoolean;

        // ruleid:eqeq
        if (myBoolean == myBoolean) {
            continue;
        }

        // ruleid:eqeq
        if (myBoolean != myBoolean) {
            continue;
        }

        float someFloat;
        // ruleid:eqeq
        if (someFloat != someFloat) {
            continue;
        }
    }
}
