import javax.persistence.EntityNotFoundException;

class CatchWithoutType {
    public void foo() {
        try {
            System.out.println("foo");
        } catch (final EntityNotFoundException ignored) {
        }
    }
}
