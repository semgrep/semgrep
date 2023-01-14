for { x <- 5 if true} yield 2
for { x <- 5 } yield 2

for ( x <- 5 ) yield 2
for ( x <- 5 if true ) yield 2

for ( x <- 5 if true if false ) yield 2
for ( x <- 5
    if true if false ) yield 2
for ( x <- 5
    if true
    if false ) yield 2
for ( x <- 5
    if true
    if false
    ) yield 2

for ( x <- 5 ; if true
    if false ) yield 2
for { x <- 5 ; if true} yield 2
for ( x <- 5 ; if true ) yield 2
for { x <- 5
      x <- 6
    if true if false } yield 2
for { x <- 5
      x <- 6
    if true
    if false } yield 2
