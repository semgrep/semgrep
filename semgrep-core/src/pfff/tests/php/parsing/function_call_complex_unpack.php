<?hh // strict
// Copyright 2004-present Facebook. All Rights Reserved.

class Foo {

  public function foo():void {
    $ret = await ThriftPreparable2::gen(
      $client,
      $request->getMethodName(),
      $this->thriftTimeout,
      $request->getShard(),
      ...$request->getMethodArgs()
    );

  }
}
