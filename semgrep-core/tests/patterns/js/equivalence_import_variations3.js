// from https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/import

import defaultExport from "module-name";
//ERROR: yes
import * as name from "module-name";
import { export1 } from "module-name";
import { export1 as alias1 } from "module-name";
import { export1 , export2 } from "module-name";
import { foo , bar } from "module-name/path/to/specific/un-exported/file";
import { export1 } from "module-name/path/to/specific/un-exported/file";
import { export1 , export2 as alias2 } from "module-name";
import defaultExport, { export1, export2 } from "module-name";
//ERROR: yes
import defaultExport, * as name from "module-name";
import "module-name";
import {foo, bar} from '/modules/my-module.js';
import '/modules/my-module.js';

var promise = import("module-name");
let module = await import('/modules/my-module.js');
