// basic paths

export a

export a.b

export a.b.c


// this 

export this

export this.a

export a.this.b

// super 

export a.super.b

// lone metavariable

export $X

// wildcards

export a.*

export a._

export a.given

export a.given Int

// alias

export a => b

export a as b

export a as _ 

export a.b => c 

export a.b as c

// selectors

export a.b.{c, d, e}

export a.b.{c as d, e, f as g, *}
