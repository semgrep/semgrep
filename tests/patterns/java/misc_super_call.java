public class Support extends Should {

  public Troll() {
    //ERROR: match
    super(badMethod());
  }
}
