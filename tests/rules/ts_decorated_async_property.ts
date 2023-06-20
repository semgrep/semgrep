
class Foo {
  @Query()
  async foo(){
  }

  @Query()
  static async foo(){
  }

  // ruleid: decorated-async-property
  @Query()
  async bar(){
  }

  // ruleid: decorated-async-property
  @Query()
  static async bar(){
  }

  // ruleid: decorated-async-property
  @Query()
  async qux(){
  }

  // ruleid: decorated-async-property
  @Query()
  static async qux(){
  }

  @Query()
  not_async(){
  }

  @NotQuery
  async foo(){
  }

  @NotQuery
  async bar(){
  }

  @NotQuery
  async qux(){
  }
}