// https://github.com/returntocorp/semgrep/issues/3207
public class Test {

  public final ZipEntry getNextEntry() {
        String name;
        ZipEntry nextEntry = super.getNextEntry();
        if (nextEntry == null)
            return nextEntry;
        ZipEntry c = nextEntry;
        name = c.getName();
        if (name == null)
            return nextEntry;
        
        boolean b1 = !name.contains("../");
        boolean b2 = !name.contains("..\\");
        // ruleid: test
        if (b1 && b2)
            return nextEntry;

        throw new SecurityException(":" + nextEntry.getName());
    }

}
