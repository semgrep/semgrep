function bindingControls() {
  const value = source();
  callbacks(() => {
    // ruleid: binding-check
    consume(value);
  });
  callbacks((value) => {
    // ok: binding-check
    consume(value);
  });
  callbacks(({value}) => {
    // ok: binding-check
    consume(value);
  });
  callbacks(({other: value}) => {
    // ok: binding-check
    consume(value);
  });
  callbacks(({value = false}) => {
    // ok: binding-check
    consume(value);
  });
  callbacks(({value: other}) => {
    // ruleid: binding-check
    consume(value);
  });
  callbacks(() => {
    // ruleid: binding-check
    consume(value);
  });
}
