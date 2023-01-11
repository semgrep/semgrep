function foo() {

  for (let { target } = event; target && target !== this; target = target.parentNode) {
  }
}
