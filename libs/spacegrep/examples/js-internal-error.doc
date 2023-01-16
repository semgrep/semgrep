var toObj = require('../lib/objectify');

// Keep a reference to all the keys defined on the root object prototype.
var objectPrototypeKeys = toObj(Object.getOwnPropertyNames(Object.prototype));

// Keep a reference to all the keys on a function created by the function.
var functionPropertyKeys = toObj(Object.getOwnPropertyNames(function () {}));

/**
 * Check if the object has a direct property on it. Uses
 * `Object.prototype.hasOwnProperty` since the object we check against could
 * have been created using `Object.create(null)` which means it wouldn't have
 * `hasOwnProperty` on its prototype.
 *
 * @param  {Object}  object
 * @param  {String}  property
 * @return {Boolean}
 */
var _hasOwnProperty = function (object, property) {
  return Object.prototype.hasOwnProperty.call(object, property);
};

/**
 * Check if the property of the object was inherited from `Object.prototype`.
 * Please note: We can't just compare to `Object.prototype` since objects in an
 * iFrame will have inherited from a different prototype.
 *
 * @param  {Object} object
 * @param  {String} property
 * @return {Boolean}
 */
var isObjectProperty = function (object, property) {
  /**
   * Check whether the object has own property.
   *
   * @param  {String}  property
   * @return {Boolean}
   */
  var objectHasOwnProperty = function (property) {
    return _hasOwnProperty(object, property);
  };

  do {
    // Use `hasOwnProperty` from the Object's prototype since the object might
    // not have a property on it called
    if (objectHasOwnProperty(property)) {
      // Do a quick check to see if we are at the end of the prototype chain. If
      // we are, we need to compare the current object properties with
      // `Object.prototype` since we could just be at the end of a chain started
      // with `Object.create(null)`.
      if (Object.getPrototypeOf(object)) { return false; }
      // Don't check for an exact match of keys since if the prototype is from
      // an iFrame, it could have been modified by one of those irritating JS
      // developers that mess with prototypes directly.
      for (var key in objectPrototypeKeys) {
        if (_hasOwnProperty(objectPrototypeKeys, key)) {
          if (!objectHasOwnProperty(key)) {
            return false;
          }
        }
      }
      return true;
    }
  } while (object = Object.getPrototypeOf(object));

  return false;
};

/**
 * Check if the property of a function was inherited by the creation of the
 * function.
 *
 * @param  {Function} fn
 * @param  {String}   property
 * @return {Boolean}
 */
var isFunctionProperty = function (fn, property) {
  if (_hasOwnProperty(functionPropertyKeys, property)) { return true; }

  return !_hasOwnProperty(fn, property);
};

/**
 * Sets whether the property should be filter from autocompletion suggestions.
 *
 * @param  {Object}   data
 * @param  {Function} next
 */
var completionFilterPlugin = function (data, next, done) {
  var value   = data.result.value;
  var context = Object(data.context);

  if (typeof context === 'object' && isObjectProperty(context, value)) {
    return done(null, false);
  }

  if (typeof context === 'function' && isFunctionProperty(context, value)) {
    return done(null, false);
  }

  return next();
};

/**
 * Filters properties from being shown in the inspector.
 *
 * @param  {Object}   data
 * @param  {Function} next
 */
var inspectorFilterPlugin = function (data, next, done) {
  if (data.internal === '[[Prototype]]') {
    return done(null, false);
  }

  return next();
};

/**
 * A { key: function } map of all middleware used in the plugin.
 *
 * @type {Object}
 */
module.exports = {
  'inspector:filter':  inspectorFilterPlugin,
  'completion:filter': completionFilterPlugin
};
