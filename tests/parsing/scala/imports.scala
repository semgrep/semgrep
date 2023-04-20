// basic paths

import a

import a.b

import a.b.c


// this 

import this

import this.a

import a.this.b

// super 

import a.super.b

// lone metavariable

import $X

// wildcards

import a.*

import a._

import a.given

import a.given Int

// alias

import a => b

import a as b

import a as _ 

import a.b => c 

import a.b as c

// selectors

import a.b.{c, d, e}

import a.b.{c as d, e, f as g, *}
