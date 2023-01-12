// https://github.com/returntocorp/semgrep/issues/6793

package main;

public class Test {
    private boolean test = false;

    public void setTest(boolean bool) {
       this.test = bool;
    }

    public String toString() {
        if(this.test) {
            return "true";
        }
        return "false";
    }
}

