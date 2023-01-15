package error_handling.empty_catch;

public class App
{
  private void readFile() throws IOException {
    File file = new File("einput.txt");
    FileInputStream fis = new FileInputStream(file);
  }

  // ok: empty-catch
  public void ok() {
    try {
      readFile();
    }
    catch (IOException ex) {
      System.out.println(ex);
    }
  }

  public void not_ok() {
    //ERROR: match
    try {
      readFile();
    }
    catch (IOException ex) {
    }
  }
}
