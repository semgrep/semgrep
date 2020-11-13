// example from https://raw.githubusercontent.com/aviadatsnyk/node-unzipper/e1cc546622174e306e523ea741ee853daffa29d6/lib/extract.js
module.exports = Extract;

var Parse = require('./parse');
var Writer = require('fstream').Writer;
var util = require('util');
var path = require('path');

util.inherits(Extract, Parse);

function Extract (opts) {
  if (!(this instanceof Extract))
    return new Extract(opts);

  var self = this;

  Parse.call(self,opts);

  self.on('entry', function(entry) {
    if (entry.type == 'Directory') return;
    entry.pipe(Writer({
      // ruleid:path-join-resolve-traversal
      somePath: path.join(opts.path,entry.path)
    }))
    .on('error',function(e) {
      self.emit('error',e);
    });
  });
}
