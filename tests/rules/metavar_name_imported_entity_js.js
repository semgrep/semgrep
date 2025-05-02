const f1 = require('foo1')
import f2 from 'foo2'

import { bar3 } from 'foo3'
import { bar4 } from 'foo4'

// ruleid: metavar-name-imported-entity-js
f1.bar('test')

// f2 is not resolved in CE, should be foo2.default
// deepruleid: metavar-name-imported-entity-js
f2.bar('test')

// ruleid: metavar-name-imported-entity-js
bar3('test')

// ok: metavar-name-imported-entity-js
bar4('test')
