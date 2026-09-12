const defaults = (obj, ...defs) =>
  // ... is allowed not just in the last position now
  Object.assign({}, obj, ...defs.reverse(), obj);
