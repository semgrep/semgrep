const defer = (fn, ...args) => setTimeout(fn, 1, ...args);

const foo = ({ id, ...restStats }) => {};

topLevelComponents.forEach(({ id, ...restStats }) => {

});

function baz({id, ...restStats}) {
}
