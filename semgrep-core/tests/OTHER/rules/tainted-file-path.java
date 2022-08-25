// PA-1724: This was a weird regression after merging taint labels (PR #5725)
public class Foo {
    public void bar(@RequestParam String path) {
        //ruleid: tainted-file-path
        File f = new File(path);
    }
}
