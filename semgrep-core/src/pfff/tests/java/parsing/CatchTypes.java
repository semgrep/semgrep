class Foo {
   public static Fragment createFragment(String className, Bundle args) {
     try {
       final Fragment instance = (Fragment) Class.forName(className).newInstance();
       instance.setArguments(args);
       return instance;
     } catch (IllegalAccessException | InstantiationException e) {
       throw new RuntimeException("Could not instantiate " + className, e);
     } catch (ClassNotFoundException e) {
       throw new RuntimeException(className + " could not be found", e);
     } catch (ClassCastException e) {
       throw new RuntimeException("Fragment is not assignable from " + className, e);
     }
   }

}
