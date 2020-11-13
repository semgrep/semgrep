public class Bad {
          public void bad1() {
              // ruleid: cookie-issecure-false
              Cookie cookie = new Cookie("name", "value");
          }

          public void bad2() {
              // ruleid: cookie-issecure-false
              Cookie cookie = new Cookie("name", "value");
              cookie.setSecure(false);
          }
   }

 public class Ok {
          public void ok1() {
             // ok: cookie-issecure-false
             Cookie cookie = new Cookie("name", "value");
             cookie.setSecure(true);
          }
}
