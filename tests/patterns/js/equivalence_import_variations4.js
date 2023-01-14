import {x} from 'module-name';
import {y} from 'module-name';
// MATCH:
import {x, y} from 'module-name';
// MATCH:
import {y, x} from 'module-name';
// MATCH:
import {x, z, y} from 'module-name';
// MATCH:
import {a, x, b, y, c} from 'module-name';
