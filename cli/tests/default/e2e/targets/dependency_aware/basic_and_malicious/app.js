// Uses both a "basic" upgradeable dependency (lodash) and a fake malicious
// dependency (bad-lib). The call to bad() triggers the malicious rule's pattern.
const _ = require("lodash");
const bad = require("bad-lib");

_.merge({}, {});
bad();
