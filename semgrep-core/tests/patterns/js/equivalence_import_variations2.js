// from https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/import

// should not match, use 'import "module-name"' if you want to be broader
import defaultExport from "module-name";
// should not match
import * as name from "module-name";
//ERROR: yes
import { export1 } from "module-name";
//ERROR: yes
import { export1 as alias1 } from "module-name";
//ERROR: yes
import { export1 , export2 } from "module-name";
// should not match
import { foo , bar } from "module-name/path/to/specific/un-exported/file";
//ERROR: yes
import { export1 } from "module-name/path/to/specific/un-exported/file";
//ERROR: yes
import { export1 , export2 as alias2 } from "module-name";
//ERROR: yes
import defaultExport, { export1, export2 } from "module-name";
// should not match
import defaultExport, * as name from "module-name";
import "module-name";
import {foo, bar} from '/modules/my-module.js';
// side effecs only
import '/modules/my-module.js';

var promise = import("module-name");
let module = await import('/modules/my-module.js');
