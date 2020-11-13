// cf. https://github.com/janl/mustache.js/#include-templates

// e.g., browser code:
// ruleid: escape-function-overwrite
Mustache.escape = function(val) { return val; }

function renderHello() {
  var template = document.getElementById('template').innerHTML;
  var rendered = Mustache.render(template, { name: 'Luke' });
  document.getElementById('target').innerHTML = rendered;
}

// e.g., Node.js code:
function node() {
  const template = require("mustache");
  // ruleid: escape-function-overwrite
  template.escape = (t) => { return t; }
  let html = template.render(blogItem, { });
}

function ok() {
  // ok: escape-function-overwrite
  const template = require("mustache");
  let html = template.render(blogItem, { });
}
