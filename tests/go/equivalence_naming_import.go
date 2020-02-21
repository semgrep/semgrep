package Foo

import sub "subprocess"

func foo() {
  //ERROR:
  result = subprocess.open("ls")
  //ERROR:
  result = sub.open("ls")

  result = sub.not_open("ls")

}