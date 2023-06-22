
class Foo {
  @Query()
  static foo(){
  }

  @Query()
  static async foo(){
  }

  // ruleid: decorated-static-property
  @Query()
  static bar(){
  }

  // ruleid: decorated-static-property
  @Query()
  static async bar(){
  }

  // ruleid: decorated-static-property
  @Query()
  static qux(){
  }

  // ruleid: decorated-static-property
  @Query()
  static async qux(){
  }

  @Query()
  not_static(){
  }

  @NotQuery
  static foo(){
  }

  @NotQuery
  static bar(){
  }

  @NotQuery
  static qux(){
  }
}